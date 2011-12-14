{-# LANGUAGE RecordWildCards #-}

module Profiling.Linux.Perf.Parse
   ( readHeader
   , readAttributes
   , readAttributeIDs
   , readEventHeader
   , Pretty (..)
   , prettyString
   , PerfFileHeader (..)
   , PerfFileAttr (..)
   , PerfFileSection (..)
   , PerfEventHeader (..)
   ) where

import Profiling.Linux.Perf.Types

import Data.Word
import Data.Binary
import Control.Monad.Error
import System.IO
import Data.ByteString.Lazy as B
import Data.Binary.Get
import Debug.Trace
import Text.Printf
import Text.PrettyPrint

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

-- -----------------------------------------------------------------------------

class Pretty a where
   pretty :: a -> Doc

prettyString :: Pretty a => a -> String
prettyString = render . pretty

instance Pretty Word16 where
   pretty = integer . fromIntegral

instance Pretty Word32 where
   pretty = integer . fromIntegral

instance Pretty Word64 where
   pretty = integer . fromIntegral

-- Corresponds with the perf_file_section struct in <perf source>/util/header.h
data PerfFileSection
  = PerfFileSection {
       sec_offset :: Word64,   -- File offset to the section.
       sec_size   :: Word64    -- Size of the section in bytes.
    }

-- -----------------------------------------------------------------------------

instance Pretty PerfFileSection where
   pretty fs = text "offset:" <+> pretty (sec_offset fs) $$
               text "size:" <+> pretty (sec_size fs)

-- Corresponds with the perf_file_header struct in <perf source>/util/header.h
data PerfFileHeader
   = PerfFileHeader {
        fh_size          :: Word64,    -- Size of (this) header.
        fh_attr_size     :: Word64,    -- Size of one attribute section.
        fh_attrs_offset  :: Word64,    -- File offset to the attribute section.
        fh_attrs_size    :: Word64,    -- Size of the attribute section in bytes.
        fh_data_offset   :: Word64,    -- File offset to the data section.
        fh_data_size     :: Word64,    -- Size of the data section in bytes.
        fh_event_offset  :: Word64,    -- File offset to the event section.
        fh_event_size    :: Word64,    -- Size of the event section in bytes.
        fh_adds_features :: [Word32]   -- Bitfield. XXX what is this for?
     }

instance Pretty PerfFileHeader where
   pretty fh =
      text "size:" <+> pretty (fh_size fh) $$
      text "attribute size:" <+> pretty (fh_attr_size fh) $$
      text "attributes offset:" <+> pretty (fh_attrs_offset fh) $$
      text "attributes size:" <+> pretty  (fh_attrs_size fh) $$
      text "data offset:" <+> pretty (fh_data_offset fh) $$
      text "data size:" <+> pretty (fh_data_size fh) $$
      text "event offset:" <+> pretty (fh_event_offset fh) $$
      text "event size:" <+> pretty (fh_event_size fh) $$
      text "features:" <+> hsep (Prelude.map pretty $ fh_adds_features fh)

pERF_MAGIC = 0x454c494646524550 :: Word64 -- "PERFFILE"
hEADER_FEAT_BITS = (#const HEADER_FEAT_BITS) :: Int

parseFileSection :: GetEvents PerfFileSection
parseFileSection = do
    sec_offset <- getU64
    sec_size   <- getU64
    return PerfFileSection{..}

parseFileHeader :: GetEvents PerfFileHeader
parseFileHeader = do
    magic       <- getU64
    when (magic /= pERF_MAGIC) $
        throwError "incompatible file format, or not a perf file"
    fh_size        <- getU64
    fh_attr_size   <- getU64
    PerfFileSection fh_attrs_offset fh_attrs_size  <- parseFileSection
    PerfFileSection fh_data_offset  fh_data_size   <- parseFileSection
    PerfFileSection fh_event_offset fh_event_size  <- parseFileSection
    fh_adds_features <- replicateM (hEADER_FEAT_BITS `quot` 32) $ getU32
    return PerfFileHeader{..}

-- Corresponds with the perf_event_attr struct in <perf source>/util/perf_event.h
data PerfEventAttr
   = PerfEventAttr {
        ea_type :: Word32,   -- Major type: hardware/software/tracepoint/etc.
        ea_size :: Word32,   -- Size of the attr structure, for fwd/bwd compat.
        ea_config :: Word64, -- Type specific configuration information.

        -- number of events when a sample is generated if .freq
        -- is not set or frequency for sampling if .freq is set
        ea_sample_period_or_freq :: Word64,
        ea_sample_type :: Word64,        -- information about what is stored in the sampling record
        ea_read_format :: Word64,        -- XXX what is this for?
        ea_flags :: Word64,              -- this is a bitfield
        ea_wakeup_events_or_watermark :: Word32, -- wakeup every n events or bytes before wakeup
        ea_bp_type :: Word32,            -- XXX what is this for?
        ea_bp_addr_or_config1 :: Word64, -- XXX what is this for?
        ea_bp_len_or_config2 :: Word64   -- XXX what is this for?
     }

instance Pretty PerfEventAttr where
   pretty ea =
      text "type:" <+> pretty (ea_type ea) $$
      text "size:" <+> pretty (ea_size ea) $$
      text "config:" <+> pretty (ea_config ea) $$
      text "sample period or frequency:" <+> pretty (ea_sample_period_or_freq ea) $$
      text "sample type:" <+> pretty (ea_sample_type ea) $$
      text "read format:" <+> pretty (ea_read_format ea) $$
      text "flags: " <+> pretty (ea_flags ea) $$
      text "wakeup events or watermark:" <+> pretty (ea_wakeup_events_or_watermark ea) $$
      text "bp type:" <+> pretty (ea_bp_type ea) $$
      text "bp address or config1:" <+> pretty (ea_bp_addr_or_config1 ea) $$
      text "bp length or config2:" <+> pretty (ea_bp_len_or_config2 ea)

parseEventAttr :: GetEvents PerfEventAttr
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
   return PerfEventAttr{..}

data PerfFileAttr = PerfFileAttr {
   fa_attr :: PerfEventAttr,
   fa_ids_offset :: Word64, -- File offset to the ids section.
   fa_ids_size   :: Word64  -- Size of the ids section in bytes.
}

instance Pretty PerfFileAttr where
   pretty fa =
      text "event attribute:" <+> pretty (fa_attr fa) $$
      text "ids offset:" <+> pretty (fa_ids_offset fa) $$
      text "ids size:" <+> pretty (fa_ids_size fa)

parseFileAttr :: GetEvents PerfFileAttr
parseFileAttr = do
  fa_attr <- parseEventAttr
  PerfFileSection fa_ids_offset fa_ids_size <- parseFileSection
  return PerfFileAttr{..}

-- Corresponds with the perf_event_header struct in <perf source>/util/perf_event.h
data PerfEventHeader = PerfEventHeader {
   eh_type :: Word32,
   eh_misc :: Word16,
   eh_size :: Word16
}

instance Pretty PerfEventHeader where
   pretty eh =
      text "type:" <+> pretty (eh_type eh) $$
      text "misc:" <+> pretty (eh_misc eh) $$
      text "size:" <+> pretty (eh_size eh)

parseEventHeader :: GetEvents PerfEventHeader
parseEventHeader = do
   eh_type <- getU32
   eh_misc <- getU16
   eh_size <- getU16
   return PerfEventHeader{..}

-- Corresponds with the comm_event struct in <perf source>/util/event.h (without the header)
data CommEvent = CommEvent {
   ce_pid :: Word32,  -- process id
   ce_tid :: Word32,  -- thread id
   ce_comm :: String  -- name of the application
}

instance Pretty CommEvent where
   pretty ce =
      text "pid:" <+> pretty (ce_pid ce) $$
      text "tid:" <+> pretty (ce_tid ce) $$
      text "comm:" <+> text (ce_comm ce)

-- Corresponds with the mmap_event struct in <perf source>/util/event.h (without the header)
data MmapEvent = MmapEvent {
   me_pid :: Word32,
   me_tid :: Word32,
   me_start :: Word64,
   me_len :: Word64,
   me_pgoff :: Word64,
   me_filename :: String
}

instance Pretty MmapEvent where
   pretty me =
      text "pid:" <+> pretty (me_pid me) $$
      text "tid:" <+> pretty (me_tid me) $$
      text "start:" <+> pretty (me_start me) $$
      text "len:" <+> pretty (me_len me) $$
      text "pgoff:" <+> pretty (me_pgoff me) $$
      text "filename:" <+> text (me_filename me)

-- -----------------------------------------------------------------------------

readEventHeader :: Handle -> Word64 -> IO PerfEventHeader
readEventHeader h offset = do
   b <- B.hGet h (#size struct perf_event_header)
   runGetEventsCheck parseEventHeader b 

readHeader :: Handle -> IO PerfFileHeader
readHeader h = do
   b <- B.hGet h (#size struct perf_file_header)
   runGetEventsCheck parseFileHeader b

readAttributes :: Handle -> PerfFileHeader -> IO [PerfFileAttr]
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

readAttributeIDs :: Handle -> PerfFileAttr -> IO [Word64]
readAttributeIDs h attr = do
   let offset = fromIntegral $ fa_ids_offset attr
       size = fromIntegral $ fa_ids_size attr
   hSeek h AbsoluteSeek offset
   b <- B.hGet h (size * bytesInWord64)
   runGetEventsCheck (replicateM size getU64) b
