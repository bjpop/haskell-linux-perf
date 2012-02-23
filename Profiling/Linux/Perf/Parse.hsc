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
   , readEventTypes
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

bytesInWord64 :: Int
bytesInWord64 = sizeOf (undefined :: Word64)

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

-- -----------------------------------------------------------------------------

-- from <perf source>/util/header.h 
--
-- struct perf_file_section {
--      u64 offset;
--      u64 size;
-- };

parseFileSection :: GetEvents FileSection
parseFileSection = do
    sec_offset <- getU64
    sec_size   <- getU64
    return FileSection{..}

-- from <perf source>/util/header.h 
--
-- struct perf_file_header {
--      u64                             magic;
--      u64                             size;
--      u64                             attr_size;
--      struct perf_file_section        attrs;
--      struct perf_file_section        data;
--      struct perf_file_section        event_types;
--      DECLARE_BITMAP(adds_features, HEADER_FEAT_BITS);
-- };

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

-- from <system include directory>/linux/perf_event.h
--
-- struct perf_event_attr {
--
--      /*
--       * Major type: hardware/software/tracepoint/etc.
--       */
--      __u32                   type;
--
--      /*
--       * Size of the attr structure, for fwd/bwd compat.
--       */
--      __u32                   size;
--
--      /*
--       * Type specific configuration information.
--       */
--      __u64                   config;
--
--      union {
--              __u64           sample_period;
--              __u64           sample_freq;
--      };
--      __u64                   sample_type;
--      __u64                   read_format;
--
--      __u64                   disabled       :  1, /* off by default        */
--                              inherit        :  1, /* children inherit it   */
--                              pinned         :  1, /* must always be on PMU */
--                              exclusive      :  1, /* only group on PMU     */
--                              exclude_user   :  1, /* don't count user      */
--                              exclude_kernel :  1, /* ditto kernel          */
--                              exclude_hv     :  1, /* ditto hypervisor      */
--                              exclude_idle   :  1, /* don't count when idle */
--                              mmap           :  1, /* include mmap data     */
--                              comm           :  1, /* include comm data     */
--                              freq           :  1, /* use freq, not period  */
--                              inherit_stat   :  1, /* per task counts       */
--                              enable_on_exec :  1, /* next exec enables     */
--                              task           :  1, /* trace fork/exit       */
--                              watermark      :  1, /* wakeup_watermark      */
--                              /*
--                               * precise_ip:
--                               *
--                               *  0 - SAMPLE_IP can have arbitrary skid
--                               *  1 - SAMPLE_IP must have constant skid
--                               *  2 - SAMPLE_IP requested to have 0 skid
--                               *  3 - SAMPLE_IP must have 0 skid
--                               *
--                               *  See also PERF_RECORD_MISC_EXACT_IP
--                               */
--                              precise_ip     :  2, /* skid constraint       */
--                              mmap_data      :  1, /* non-exec mmap data    */
--                              sample_id_all  :  1, /* sample_type all events */
--
--                              __reserved_1   : 45;
--
--      union {
--              __u32           wakeup_events;    /* wakeup every n events */
--              __u32           wakeup_watermark; /* bytes before wakeup   */
--      };
--
--      __u32                   bp_type;
--      union {
--              __u64           bp_addr;
--              __u64           config1; /* extension of config */
--      };
--      union {
--              __u64           bp_len;
--              __u64           config2; /* extension of config1 */
--      };
-- };

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

-- from <perf source>/util/header.c
--
-- struct perf_file_attr {
--      struct perf_event_attr attr;
--      struct perf_file_section ids;
-- };

parseFileAttr :: GetEvents FileAttr
parseFileAttr = do
  fa_attr <- parseEventAttr
  FileSection fa_ids_offset fa_ids_size <- parseFileSection
  return FileAttr{..}

-- from <perf source>/util/event.h
--
-- struct perf_trace_event_type {
--    u64  event_id;
--    char name[MAX_EVENT_NAME];
-- };

parseTraceEventType :: GetEvents TraceEventType
parseTraceEventType = do
  te_event_id <- getU64
  te_name <- getBSNul
  return TraceEventType{..}

-- from <system include directory>/linux/perf_event.h
--
-- struct perf_event_header {
--      __u32   type;
--      __u16   misc;
--      __u16   size;
-- };

parseEventHeader :: GetEvents EventHeader
parseEventHeader = do
   eh_type <- (toEnum . fromIntegral) `fmap` getU32
   eh_misc <- getU16
   eh_size <- getU16
   return EventHeader{..}

-- from <perf source>/util/event.h
--
-- struct mmap_event {
--      struct perf_event_header header;
--      u32 pid, tid;
--      u64 start;
--      u64 len;
--      u64 pgoff;
--      char filename[PATH_MAX];
-- };

parseMmapEvent :: GetEvents EventPayload 
parseMmapEvent = do
   -- note we do not parse the event header here, it is done in parseEvent
   me_pid <- getU32
   me_tid <- getU32
   me_start <- getU64
   me_len <- getU64
   me_pgoff <- getU64
   me_filename <- getBSNul
   return MmapEvent{..}

-- from <perf source>/util/event.h
--
-- struct comm_event {
--      struct perf_event_header header;
--      u32 pid, tid;
--      char comm[16];
-- };

parseCommEvent :: GetEvents EventPayload
parseCommEvent = do
   ce_pid <- getU32
   ce_tid <- getU32
   ce_comm <- getBSNul
   return CommEvent{..}

-- from <perf source>/util/event.h
--
-- struct fork_event {
--      struct perf_event_header header;
--      u32 pid, ppid;
--      u32 tid, ptid;
--      u64 time;
-- };

parseForkEvent :: GetEvents EventPayload
parseForkEvent = do
   fe_pid <- getU32
   fe_ppid <- getU32
   fe_tid <- getU32
   fe_ptid <- getU32
   fe_time <- getU64
   return ForkEvent{..}

-- ForkEvent and ExitEvent have the same binary structure.

parseExitEvent :: GetEvents EventPayload
parseExitEvent = do
   ee_pid <- getU32
   ee_ppid <- getU32
   ee_tid <- getU32
   ee_ptid <- getU32
   ee_time <- getU64
   return ExitEvent{..}

-- from <perf source>/util/event.h
--
-- struct lost_event {
--      struct perf_event_header header;
--      u64 id;
--      u64 lost;
-- };

parseLostEvent :: GetEvents EventPayload
parseLostEvent = do
   le_id <- getU64
   le_lost <- getU64
   return LostEvent{..}

-- from <system include directory>/linux/perf_event.h
-- Note: cannnot find corresponding entry in <perf source>/util/event.h

-- struct {
--      struct perf_event_header        header;
--      u64                             time;
--      u64                             id;
--      u64                             stream_id;
-- };

parseThrottleEvent :: GetEvents EventPayload
parseThrottleEvent = do
   te_time <- getU64
   te_id <- getU64
   te_stream_id <- getU64
   return ThrottleEvent{..}

parseUnThrottleEvent :: GetEvents EventPayload
parseUnThrottleEvent = do
   ue_time <- getU64
   ue_id <- getU64
   ue_stream_id <- getU64
   return UnThrottleEvent{..}

-- from <perf source>/util/event.h
--
-- struct read_event {
--      struct perf_event_header header;
--      u32 pid, tid;
--      u64 value;
--      u64 time_enabled;
--      u64 time_running;
--      u64 id;
-- };

parseReadEvent :: GetEvents EventPayload
parseReadEvent = do
   re_pid <- getU32
   re_tid <- getU32
   re_value <- getU64
   re_time_enabled <- getU64
   re_time_running <- getU64
   re_id <- getU64
   return ReadEvent{..}

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

-- XXX sample_id_all is not handled yet.
-- if this flag is set to TRUE then events other than SampleEvent
-- have additional information. See perf_event__parse_sample
-- in cern_perf/readperf/origperf.c and also perf_event__parse_id_sample.
-- We use ea_sample_type from EventAttr to determine what sampling
-- information we have.

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
      PERF_RECORD_UNKNOWN _ -> return UnknownEvent

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

readAttributeIDs :: Handle -> FileAttr -> IO [Word64]
readAttributeIDs h attr = do
   let offset = fromIntegral $ fa_ids_offset attr
       size = fromIntegral $ fa_ids_size attr
   hSeek h AbsoluteSeek offset
   -- b <- B.hGet h (size * bytesInWord64)
   b <- B.hGet h size
   runGetEventsCheck (replicateM (size `div` bytesInWord64) getU64) b

{-
readEventTypes :: Handle -> FileHeader -> IO [TraceEventType]
readEventTypes h fh = do
   let sizeOfTypeRecord = #size struct perf_trace_event_type
   let nr_types = fh_event_size fh `quot` sizeOfTypeRecord
   hSeek h AbsoluteSeek (fromIntegral (fh_event_offset fh))
   b <- B.hGet h (fromIntegral (fh_event_size fh))
   runGetEventsCheck (replicateM (fromIntegral nr_types) parseTraceEventType) b
-}

readEventTypes :: Handle -> FileHeader -> IO [TraceEventType]
readEventTypes h fh = do
   -- hSeek h AbsoluteSeek (fromIntegral (fh_event_offset fh))
   -- b <- B.hGet h (fromIntegral (fh_event_size fh))
   -- runGetEventsCheck (replicateM (fromIntegral nr_types) parseTraceEventType) b
   hSeek h AbsoluteSeek (fromIntegral (fh_event_offset fh))
   loop nr_types []
   where
   loop 0 acc = return $ reverse acc
   loop n acc = do
      -- hSeek h AbsoluteSeek offset
      b <- B.hGet h sizeOfTypeRecord
      nextRecord <- runGetEventsCheck parseTraceEventType b
      loop (n-1) (nextRecord:acc)
   sizeOfTypeRecord :: Int
   sizeOfTypeRecord = fromIntegral (#size struct perf_trace_event_type)
   nr_types = (fromIntegral $ fh_event_size fh) `quot` sizeOfTypeRecord
