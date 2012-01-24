{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope 
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A library to parse and pretty print the contents of "perf.data" file
-- "perf.data" is the the output of the "perf record" command on
-- linux (linux performance counter information).
--
-----------------------------------------------------------------------------

module Profiling.Linux.Perf.Parse
   ( readHeader
   , readAttributes
   , readAttributeIDs
   , readEventHeader
   , readEvent
   , module Types
   ) where

import Profiling.Linux.Perf.Types as Types
import Data.Word
import Data.Binary
import Control.Monad.Error
import System.IO
import Data.ByteString.Lazy as B (ByteString, hGet)
import Data.Binary.Get
   (Get, runGet, getLazyByteStringNul, getWord16le, getWord32le, getWord64le)
import Debug.Trace
import Text.Printf
import Data.Bits (testBit)
import Foreign.Storable (sizeOf)

#include <linux/perf_event.h>
#include "perf_file.h"

-- -----------------------------------------------------------------------------

type GetEvents a = ErrorT String Get a

getE :: Binary a => GetEvents a
getE = lift get

-- read a null terminated (lazy) byte string
getBSNul :: GetEvents B.ByteString
getBSNul = lift getLazyByteStringNul

-- read an unsigned 8 bit word
getU8 :: GetEvents Word8
getU8 = lift getWord8

-- read an unsigned 16 bit word
getU16 :: GetEvents Word16
getU16 = lift getWord16le

-- read an unsigned 32 bit word
getU32 :: GetEvents Word32
getU32 = lift getWord32le

-- read an unsigned 64 bit word
getU64 :: GetEvents Word64
getU64 = lift getWord64le

runGetEvents :: GetEvents a -> B.ByteString -> Either String a
runGetEvents = runGet . runErrorT

runGetEventsCheck  :: GetEvents a -> B.ByteString -> IO a
runGetEventsCheck g b =
   case runGetEvents g b of
      Left e -> fail e
      Right v -> return v

-- magic 8 bytes at the start of the perf file, "PERFFILE"
pERF_MAGIC = 0x454c494646524550 :: Word64

hEADER_FEAT_BITS = (#const HEADER_FEAT_BITS) :: Int

parseFileSection :: GetEvents FileSection
parseFileSection = do
    sec_offset <- getU64
    sec_size   <- getU64
    return FileSection{..}

parseFileHeader :: GetEvents FileHeader
parseFileHeader = do
    magic       <- getU64
    when (magic /= pERF_MAGIC) $
        throwError "incompatible file format, or not a perf file"
    fh_size        <- getU64
    fh_attr_size   <- getU64
    FileSection fh_attrs_offset fh_attrs_size  <- parseFileSection
    FileSection fh_data_offset  fh_data_size   <- parseFileSection
    FileSection fh_event_offset fh_event_size  <- parseFileSection
    fh_adds_features <- replicateM (hEADER_FEAT_BITS `quot` 32) $ getU32
    return FileHeader{..}

parseEventAttr :: GetEvents EventAttr
parseEventAttr = do
   ea_type <- getU32
   ea_size <- getU32
   ea_config <- getU64
   ea_sample_period_or_freq <- getU64
   ea_sample_type <- getU64
   ea_read_format <- getU64
   ea_flags <- getU64
   ea_wakeup_events_or_watermark <- getU32
   ea_bp_type <- getU32
   ea_bp_addr_or_config1 <- getU64
   ea_bp_len_or_config2 <- getU64
   return EventAttr{..}

parseFileAttr :: GetEvents FileAttr
parseFileAttr = do
  fa_attr <- parseEventAttr
  FileSection fa_ids_offset fa_ids_size <- parseFileSection
  return FileAttr{..}

parseEventHeader :: GetEvents EventHeader
parseEventHeader = do
   eh_type <- (toEnum . fromIntegral) `fmap` getU32
   eh_misc <- getU16
   eh_size <- getU16
   return EventHeader{..}

parseMmapEvent :: GetEvents EventPayload 
parseMmapEvent = do
   me_pid <- getU32
   me_tid <- getU32
   me_start <- getU64
   me_len <- getU64
   me_pgoff <- getU64
   me_filename <- getBSNul
   return MmapEvent{..}

parseCommEvent :: GetEvents EventPayload
parseCommEvent = do
   ce_pid <- getU32
   ce_tid <- getU32
   ce_comm <- getBSNul
   return CommEvent{..}

parseForkEvent :: GetEvents EventPayload
parseForkEvent = do
   fe_pid <- getU32
   fe_ppid <- getU32
   fe_tid <- getU32
   fe_ptid <- getU32
   fe_time <- getU64
   return ForkEvent{..}

parseExitEvent :: GetEvents EventPayload
parseExitEvent = do
   ee_pid <- getU32
   ee_ppid <- getU32
   ee_tid <- getU32
   ee_ptid <- getU32
   ee_time <- getU64
   return ExitEvent{..}

parseLostEvent :: GetEvents EventPayload
parseLostEvent = do
   le_id <- getU64
   le_lost <- getU64
   return LostEvent{..}

parseThrottleEvent = error "parseThrottleEvent"
parseUnThrottleEvent = error "parseUnThrottleEvent"
parseReadEvent = error "parseReadEvent"

parseSampleType :: Word64 -> SampleFormat -> GetEvents a -> GetEvents (Maybe a)
parseSampleType sampleType format parser
   | testBit sampleType (fromEnum format) = Just `fmap` parser
   | otherwise = return Nothing

parseSampleEvent :: Word64 -> GetEvents EventPayload
parseSampleEvent sampleType = do
   se_ip <- parseSampleType sampleType PERF_SAMPLE_IP getU64
   se_pid <- parseSampleType sampleType PERF_SAMPLE_TID getU32
   se_tid <- parseSampleType sampleType PERF_SAMPLE_TID getU32
   se_time <- parseSampleType sampleType PERF_SAMPLE_TIME getU64
   se_addr <- parseSampleType sampleType PERF_SAMPLE_ADDR getU64
   se_id <- parseSampleType sampleType PERF_SAMPLE_ID getU64
   se_streamid <- parseSampleType sampleType PERF_SAMPLE_STREAM_ID getU64
   se_cpu <- parseSampleType sampleType PERF_SAMPLE_CPU getU32
   se_period <- parseSampleType sampleType PERF_SAMPLE_PERIOD getU64
   return SampleEvent{..}

parseEventPayload :: Word64 -> EventType -> GetEvents EventPayload
parseEventPayload sampleType eventType =
   case eventType of
      PERF_RECORD_MMAP -> parseMmapEvent
      PERF_RECORD_LOST -> parseLostEvent
      PERF_RECORD_COMM -> parseCommEvent
      PERF_RECORD_EXIT -> parseExitEvent
      PERF_RECORD_THROTTLE -> parseThrottleEvent
      PERF_RECORD_UNTHROTTLE -> parseUnThrottleEvent
      PERF_RECORD_FORK -> parseForkEvent
      PERF_RECORD_READ -> parseReadEvent
      PERF_RECORD_SAMPLE -> parseSampleEvent sampleType

parseEvent :: Word64 -> GetEvents Event
parseEvent sampleType = do
   ev_header <- parseEventHeader
   let eventType = eh_type ev_header
   ev_payload <- parseEventPayload sampleType eventType
   return Event{..}

-- -----------------------------------------------------------------------------

readEventHeader :: Handle -> Word64 -> IO EventHeader
readEventHeader h offset = do
   hSeek h AbsoluteSeek $ fromIntegral offset
   b <- B.hGet h (#size struct perf_event_header)
   runGetEventsCheck parseEventHeader b 

readEvent :: Handle -> Word64 -> Word64 -> IO Event
readEvent h offset sampleType = do
   hSeek h AbsoluteSeek $ fromIntegral offset
   let headerSize = #size struct perf_event_header
   headerBytes <- B.hGet h headerSize
   ev_header <- runGetEventsCheck parseEventHeader headerBytes
   let payloadSize = (fromIntegral $ eh_size ev_header) - headerSize
   payloadBytes <- B.hGet h payloadSize
   ev_payload <- runGetEventsCheck (parseEventPayload sampleType $ eh_type ev_header) payloadBytes
   return Event{..}

readHeader :: Handle -> IO FileHeader
readHeader h = do
   b <- B.hGet h (#size struct perf_file_header)
   runGetEventsCheck parseFileHeader b

readAttributes :: Handle -> FileHeader -> IO [FileAttr]
readAttributes h fh = do
   -- XXX I wonder if this calculation should be:
   -- fh_attrs_size fh `quot` fh_attr_size fh ?
   let nr_attrs = fh_attrs_size fh `quot` (#size struct perf_file_attr)
   hSeek h AbsoluteSeek (fromIntegral (fh_attrs_offset fh))
   b <- B.hGet h (fromIntegral (fh_attrs_size fh))
   runGetEventsCheck (replicateM (fromIntegral nr_attrs) parseFileAttr) b

bytesInWord64 :: Int
bytesInWord64 = sizeOf (undefined :: Word64)

readAttributeIDs :: Handle -> FileAttr -> IO [Word64]
readAttributeIDs h attr = do
   let offset = fromIntegral $ fa_ids_offset attr
       size = fromIntegral $ fa_ids_size attr
   hSeek h AbsoluteSeek offset
   b <- B.hGet h (size * bytesInWord64)
   runGetEventsCheck (replicateM size getU64) b
