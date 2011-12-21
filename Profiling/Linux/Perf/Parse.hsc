{-# LANGUAGE RecordWildCards #-}

module Profiling.Linux.Perf.Parse
   ( readHeader
   , readAttributes
   , readAttributeIDs
   , readEventHeader
   , module Types
   ) where

import Profiling.Linux.Perf.Types as Types
import Data.Word
import Data.Binary
import Control.Monad.Error
import System.IO
import Data.ByteString.Lazy as B
import Data.Binary.Get
import Debug.Trace
import Text.Printf

#include <linux/perf_event.h>
#include "perf_file.h"

-- -----------------------------------------------------------------------------

type GetEvents a = ErrorT String Get a

getE :: Binary a => GetEvents a
getE = lift get

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

pERF_MAGIC = 0x454c494646524550 :: Word64 -- "PERFFILE"
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
   eh_type <- getU32
   eh_misc <- getU16
   eh_size <- getU16
   return EventHeader{..}

readMmapEvent :: GetEvents EventPayload 
readMmapEvent = do
   me_pid <- getU32
   me_tid <- getU32
   me_start <- getU64
   me_len <- getU64
   me_pgoff <- getU64
   let me_filename = "foobar" -- XXX fixme
   return MmapEvent{..}

readForkEvent :: GetEvents EventPayload
readForkEvent = do
   fe_pid <- getU32
   fe_ppid <- getU32
   fe_tid <- getU32
   fe_ptid <- getU32
   fe_time <- getU64
   return ForkEvent{..}

readExitEvent :: GetEvents EventPayload
readExitEvent = do
   ee_pid <- getU32
   ee_ppid <- getU32
   ee_tid <- getU32
   ee_ptid <- getU32
   ee_time <- getU64
   return ExitEvent{..}

readLostEvent :: GetEvents EventPayload
readLostEvent = do
   le_id <- getU64
   le_lost <- getU64
   return LostEvent{..}

-- -----------------------------------------------------------------------------

readEventHeader :: Handle -> Word64 -> IO EventHeader
readEventHeader h offset = do
   hSeek h AbsoluteSeek (fromIntegral offset)
   b <- B.hGet h (#size struct perf_event_header)
   runGetEventsCheck parseEventHeader b 

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
   b <- hGet h (fromIntegral (fh_attrs_size fh))
   runGetEventsCheck (replicateM (fromIntegral nr_attrs) parseFileAttr) b

-- XXX this should be calculated
bytesInWord64 :: Int
bytesInWord64 = 8

readAttributeIDs :: Handle -> FileAttr -> IO [Word64]
readAttributeIDs h attr = do
   let offset = fromIntegral $ fa_ids_offset attr
       size = fromIntegral $ fa_ids_size attr
   hSeek h AbsoluteSeek offset
   b <- B.hGet h (size * bytesInWord64)
   runGetEventsCheck (replicateM size getU64) b
