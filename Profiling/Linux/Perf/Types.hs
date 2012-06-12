{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Types for representing the parsed contents of a @perf.data@ file output
-- the @perf record@ command on Linux (Linux performance counter information).
-- 
-- There is an intentional close correspondence between the types in this
-- module and the representation in the C implementation of perf. 
--
-----------------------------------------------------------------------------

module Profiling.Linux.Perf.Types
   ( -- * Identifiers
     PID (..)
   , TID (..)
   , EventID (..)
   , EventTypeID (..)

   -- * Byte counters

   , ByteCount64 (..)
   , ByteCount32 (..)
   , ByteCount16 (..)

   -- * Timestamps

   , TimeStamp (..)

   -- * File structure

   , FileSection (..)
   , FileHeader (..)
   , FileAttr (..)

   -- * Event data

   , Event (..)
   , EventCPUMode (..)
   , EventAttr (..)
   , EventHeader (..)
   , EventPayload (..)

   -- * Event type information

   , EventType (..)
   , SampleFormat (..)
   , TraceEventType (..)
   , EventAttrFlag (..)
   , EventSource (..)
   , SampleTypeBitMap (..)
   , testEventAttrFlag

   -- * An abstract representation of the perf data file

   , PerfData (..)
   ) where

import Data.Word (Word64, Word32, Word16, Word8, Word)
import Text.PrettyPrint (text, (<+>), ($$), render, empty, integer, (<>), hsep, Doc)
import Data.ByteString.Lazy (ByteString)
import Profiling.Linux.Perf.Pretty (Pretty (..), showBits)
import Data.Bits (testBit)

-- -----------------------------------------------------------------------------

-- | The various parts of the perf.data file collected together.
data PerfData =
   PerfData
   { perfData_fileHeader :: FileHeader -- ^ File header explains the structure of the file.
   , perfData_attrs :: [FileAttr] -- ^ Common attributes of events.
   , perfData_idss :: [[EventID]] -- ^ Event identifiers to be associated with the event attributes.
   , perfData_types :: [TraceEventType] -- ^ Event type information.
   , perfData_events :: [Event] -- ^ The event payload.
   }

-- | Process ID.
newtype PID = PID { pid :: Word32 }
   deriving (Eq, Ord, Show, Pretty)

-- | Thread ID.
newtype TID = TID { tid :: Word32 }
   deriving (Eq, Ord, Show, Pretty)

-- | Event type ID (magic unique number of an event type).
newtype EventTypeID = EventTypeID { eventTypeID :: Word64 }
   deriving (Eq, Ord, Show, Pretty)

-- | Event ID.
-- Not really an identity. This number is used to link
-- an event to an event type. Multiple events can have the same EventID,
-- which means they all have the same event type.
newtype EventID = EventID { eventID :: Word64 }
   deriving (Eq, Ord, Show, Pretty)

-- | Measurement of time passed in nanoseconds since a given point.
newtype TimeStamp = TimeStamp { timeStamp :: Word64 }
   deriving (Eq, Ord, Show, Pretty)

-- | A bitmap encoding information about the content of sample events.
newtype SampleTypeBitMap = SampleTypeBitMap { sampleTypeBitMap :: Word64 }
   deriving (Eq, Show, Pretty)

-- | A 64 bit measurement in bytes. For example the size of an object, or an offset.
newtype ByteCount64 = ByteCount64 { byteCount64 :: Word64 }
   deriving (Eq, Ord, Show, Pretty, Enum, Integral, Real, Num)

-- | A 32 bit measurement in bytes. For example the size of an object, or an offset.
newtype ByteCount32 = ByteCount32 { byteCount32 :: Word32 }
   deriving (Eq, Ord, Show, Pretty, Enum, Integral, Real, Num)

-- | A 16 bit measurement in bytes. For example the size of an object, or an offset.
newtype ByteCount16 = ByteCount16 { byteCount16 :: Word16 }
   deriving (Eq, Ord, Show, Pretty, Enum, Integral, Real, Num)

-- | A Single event record.
data Event =
   Event
   { ev_header :: EventHeader -- ^ Information about the structure of the event.
   , ev_payload :: EventPayload -- ^ The event data.
   }

instance Pretty Event where
   pretty ev@(Event {}) =
      text "header: " <+> (pretty $ ev_header ev) $$
      text "payload: " <+> (pretty $ ev_payload ev)

-- XXX should probably get this from the definintion of the C type
-- | Encoding of the @perf_event_header->type@.
data EventType 
   = PERF_RECORD_MMAP       -- 1
   | PERF_RECORD_LOST       -- 2
   | PERF_RECORD_COMM       -- 3
   | PERF_RECORD_EXIT       -- 4
   | PERF_RECORD_THROTTLE   -- 5
   | PERF_RECORD_UNTHROTTLE -- 6
   | PERF_RECORD_FORK       -- 7
   | PERF_RECORD_READ       -- 8
   | PERF_RECORD_SAMPLE     -- 9
   | PERF_RECORD_UNKNOWN Int  -- possibly a bad number?
   deriving (Eq, Show)

instance Enum EventType where

   toEnum 1 = PERF_RECORD_MMAP
   toEnum 2 = PERF_RECORD_LOST
   toEnum 3 = PERF_RECORD_COMM
   toEnum 4 = PERF_RECORD_EXIT
   toEnum 5 = PERF_RECORD_THROTTLE
   toEnum 6 = PERF_RECORD_UNTHROTTLE
   toEnum 7 = PERF_RECORD_FORK
   toEnum 8 = PERF_RECORD_READ
   toEnum 9 = PERF_RECORD_SAMPLE
   toEnum other = PERF_RECORD_UNKNOWN other

   fromEnum PERF_RECORD_MMAP = 1
   fromEnum PERF_RECORD_LOST = 2
   fromEnum PERF_RECORD_COMM = 3
   fromEnum PERF_RECORD_EXIT = 4
   fromEnum PERF_RECORD_THROTTLE = 5
   fromEnum PERF_RECORD_UNTHROTTLE = 6
   fromEnum PERF_RECORD_FORK = 7
   fromEnum PERF_RECORD_READ = 8
   fromEnum PERF_RECORD_SAMPLE = 9
   fromEnum (PERF_RECORD_UNKNOWN other) = other

instance Pretty EventType where
   pretty = text . show

-- | Information about what is stored in a sample event.
data SampleFormat
   = PERF_SAMPLE_IP        -- ^ 1U << 0
   | PERF_SAMPLE_TID       -- ^ 1U << 1
   | PERF_SAMPLE_TIME      -- ^ 1U << 2
   | PERF_SAMPLE_ADDR      -- ^ 1U << 3
   | PERF_SAMPLE_READ      -- ^ 1U << 4
   | PERF_SAMPLE_CALLCHAIN -- ^ 1U << 5
   | PERF_SAMPLE_ID        -- ^ 1U << 6
   | PERF_SAMPLE_CPU       -- ^ 1U << 7
   | PERF_SAMPLE_PERIOD    -- ^ 1U << 8
   | PERF_SAMPLE_STREAM_ID -- ^ 1U << 9
   | PERF_SAMPLE_RAW       -- ^ 1U << 10
   deriving (Eq, Enum, Show)

instance Pretty SampleFormat where
   pretty = text . show

-- | A bitfield in @perf_event_header->misc@.
data EventCPUMode 
   = PERF_RECORD_CPUMODE_UNKNOWN -- ^ 0
   | PERF_RECORD_MISC_KERNEL     -- ^ 1
   | PERF_RECORD_MISC_USER       -- ^ 2
   | PERF_RECORD_MISC_HYPERVISOR -- ^ 3
   deriving (Eq, Show)

instance Pretty EventCPUMode where
   pretty = text . show

-- | Corresponds with the @perf_file_section@ struct in @\<perf source\>\/util\/header.h@
data FileSection
  = FileSection {
       sec_offset :: ByteCount64, -- ^ File offset to the section.
       sec_size   :: ByteCount64  -- ^ Size of the section in bytes.
    }

instance Pretty FileSection where
   pretty fs = text "offset:" <+> pretty (sec_offset fs) $$
               text "size:" <+> pretty (sec_size fs)

-- | Corresponds with the @perf_file_header@ struct in @\<perf source\>\/util\/header.h@
data FileHeader
   = FileHeader {
        fh_size          :: ByteCount64,    -- ^ Size of (this) header.
        fh_attr_size     :: ByteCount64,    -- ^ Size of one attribute section.
        fh_attrs_offset  :: ByteCount64,    -- ^ File offset to the attribute section.
        fh_attrs_size    :: ByteCount64,    -- ^ Size of the attribute section in bytes.
        fh_data_offset   :: ByteCount64,    -- ^ File offset to the data section.
        fh_data_size     :: ByteCount64,    -- ^ Size of the data section in bytes.
        fh_event_offset  :: ByteCount64,    -- ^ File offset to the event section.
        fh_event_size    :: ByteCount64,    -- ^ Size of the event section in bytes.
        fh_adds_features :: [Word32]        -- ^ Bitfield.
     }

instance Pretty FileHeader where
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

-- | See struct @perf_event_attr@ in @linux\/perf_event.h@
data EventAttrFlag
   = Disabled              -- ^ off by default
   | Inherit               -- ^ children inherit it
   | Pinned                -- ^ must always be on PMU
   | Exclusive             -- ^ only group on PMU
   | ExcludeUser           -- ^ don't count user
   | ExcludeKernel         -- ^ ditto kernel
   | ExcludeHV             -- ^ ditto hypervisor
   | ExcludeIdle           -- ^ don't count when idle
   | Mmap                  -- ^ include mmap data
   | Comm                  -- ^ include comm data
   | Freq                  -- ^ use freq, not period
   | InheritStat           -- ^ per task counts
   | EnableOnExec          -- ^ next exec enables
   | Task                  -- ^ trace fork/exit
   | WaterMark             -- ^ wakeup_watermark

   
   | ArbitrarySkid         -- ^ precise_ip, See also @PERF_RECORD_MISC_EXACT_IP@
   | ConstantSkid          -- ^ precise_ip, See also @PERF_RECORD_MISC_EXACT_IP@
   | RequestedZeroSkid     -- ^ precise_ip, See also @PERF_RECORD_MISC_EXACT_IP@
   | CompulsoryZeroSkid    -- ^ precise_ip, See also @PERF_RECORD_MISC_EXACT_IP@

   | MmapData              -- ^ non-exec mmap data
   | SampleIdAll           -- ^ sample_type all events
   deriving (Eq, Ord, Enum, Show)

instance Pretty EventAttrFlag where
   pretty Disabled = text "disabled"
   pretty Inherit = text "inherit"
   pretty Pinned = text "pinned"
   pretty Exclusive = text "exclusive"
   pretty ExcludeUser = text "exclude-user"
   pretty ExcludeKernel = text "exclude-kernel"
   pretty ExcludeHV = text "exclude-hypervisor"
   pretty ExcludeIdle = text "exclude-idle"
   pretty Mmap = text "mmap"
   pretty Comm = text "comm"
   pretty Freq = text "freq"
   pretty InheritStat = text "inherit-stat"
   pretty EnableOnExec = text "enable-on-exec"
   pretty Task = text "task"
   pretty WaterMark = text "watermark"
   pretty ArbitrarySkid = text "arbitrary-skid"
   pretty ConstantSkid = text "constant-skid"
   pretty RequestedZeroSkid = text "requested-zero-skid"
   pretty CompulsoryZeroSkid = text "compulsory-zero-skid"
   pretty MmapData = text "mmap-data"
   pretty SampleIdAll = text "sample-id-all"

-- | Test if a given EventAttrFlag is set.

-- Tedious definition because of the way the skid flags are
-- implemented as a 2 bit word instead of individual single bits.
testEventAttrFlag :: Word64 -> EventAttrFlag -> Bool
testEventAttrFlag word flag =
   case flag of
      Disabled           -> testBit word 0
      Inherit            -> testBit word 1
      Pinned             -> testBit word 2
      Exclusive          -> testBit word 3
      ExcludeUser        -> testBit word 4
      ExcludeKernel      -> testBit word 5
      ExcludeHV          -> testBit word 6
      ExcludeIdle        -> testBit word 7
      Mmap               -> testBit word 8
      Comm               -> testBit word 9
      Freq               -> testBit word 10
      InheritStat        -> testBit word 11
      EnableOnExec       -> testBit word 12
      Task               -> testBit word 13
      WaterMark          -> testBit word 14
      ArbitrarySkid      -> not (testBit word 15) && not (testBit word 16)
      ConstantSkid       -> not (testBit word 15) && (testBit word 16)
      RequestedZeroSkid  -> (testBit word 15) && not (testBit word 16)
      CompulsoryZeroSkid -> (testBit word 15) && (testBit word 16)
      MmapData           -> testBit word 17
      SampleIdAll        -> testBit word 18

-- | Pretty print an event attribute bit field as a list of set flags.
prettyFlags :: Word64 -> Doc
prettyFlags word = foldr testFlag empty [toEnum 0 ..]
   where
   testFlag :: EventAttrFlag -> Doc -> Doc
   testFlag flag rest
      | testEventAttrFlag word flag = pretty flag <+> rest
      | otherwise = rest

-- | Corresponds with the enum @perf_type_id@ in @include\/linux\/perf_event.h@

-- XXX should really derive this directly from the header file
data EventSource
   = PerfTypeHardware     -- ^ 0
   | PerfTypeSoftware     -- ^ 1
   | PerfTypeTracePoint   -- ^ 2
   | PerfTypeHwCache      -- ^ 3
   | PerfTypeRaw          -- ^ 4
   | PerfTypeBreakpoint   -- ^ 5
   | PerfTypeUnknown
   deriving (Eq, Ord, Show, Enum)

instance Pretty EventSource where
   pretty = text . show

-- | Corresponds with the @perf_event_attr@ struct in @include\/linux\/perf_event.h@
data EventAttr
   = EventAttr {
        ea_type :: EventSource,   -- ^ Major type: hardware/software/tracepoint/etc.
                                  -- defined as enum perf_type_id in include/linux/perf_event.h
        ea_size :: ByteCount32,   -- ^ Size of the attr structure, for fwd/bwd compat.
        ea_config :: EventTypeID, -- ^ Link to .event id of perf trace event type.

        
        ea_sample_period_or_freq :: Word64, -- ^ Number of events when a sample is generated if .freq
                                            -- is not set or frequency for sampling if .freq is set.
        ea_sample_type :: SampleTypeBitMap, -- ^ Information about what is stored in the sampling record.
        ea_read_format :: Word64,           -- 
        ea_flags :: Word64,                 -- 
        ea_wakeup_events_or_watermark :: Word32, -- ^ Wakeup every n events or bytes before wakeup.
        ea_bp_type :: Word32,               -- 
        ea_bp_addr_or_config1 :: Word64,    -- 
        ea_bp_len_or_config2 :: Word64      -- 
     }

instance Pretty EventAttr where
   pretty ea =
      text "type:" <+> pretty (ea_type ea) $$
      text "size:" <+> pretty (ea_size ea) $$
      text "config:" <+> pretty (ea_config ea) $$
      text "sample period or frequency:" <+> pretty (ea_sample_period_or_freq ea) $$
      text "sample type:" <+> pretty (ea_sample_type ea) $$
      text "read format:" <+> pretty (ea_read_format ea) $$
      text "flags:" <+> prettyFlags (ea_flags ea) $$
      text "wakeup events or watermark:" <+> pretty (ea_wakeup_events_or_watermark ea) $$
      text "bp type:" <+> pretty (ea_bp_type ea) $$
      text "bp address or config1:" <+> pretty (ea_bp_addr_or_config1 ea) $$
      text "bp length or config2:" <+> pretty (ea_bp_len_or_config2 ea)


-- | Layout of event attribute and attribute ids.
data FileAttr = FileAttr {
   fa_attr :: EventAttr,         -- ^ The attribute payload.
   fa_ids_offset :: ByteCount64, -- ^ File offset to the ids section.
   fa_ids_size   :: ByteCount64  -- ^ Size of the ids section in bytes.
}

instance Pretty FileAttr where
   pretty fa =
      text "event attribute:" <+> pretty (fa_attr fa) $$
      text "ids offset:" <+> pretty (fa_ids_offset fa) $$
      text "ids size:" <+> pretty (fa_ids_size fa)

-- | Identity and printable name of an event type.
data TraceEventType = TraceEventType {
   te_event_id :: EventTypeID, -- ^ This entry belongs to the perf event attr entry where .config
                               -- has the same value as this id.
   te_name :: ByteString
}

instance Pretty TraceEventType where
   pretty te =
      text "event id:" <+> pretty (te_event_id te) $$
      text "name:" <+> pretty (te_name te)

-- | Corresponds with the @perf_event_header@ struct in @\<perf source\>\/util\/perf_event.h@
data EventHeader = EventHeader {
   eh_type :: EventType,
   eh_misc :: Word16,
   eh_size :: ByteCount16
}

instance Pretty EventHeader where
   pretty eh =
      text "type:" <+> pretty (eh_type eh) $$
      text "misc:" <+> pretty (eh_misc eh) $$
      text "size:" <+> pretty (eh_size eh)

data EventPayload =
   -- Corresponds with the @comm_event@ struct in @\<perf source\>\/util\/event.h@ (without the header).
   CommEvent {
      eventPayload_pid :: PID,            -- ^ process id
      eventPayload_tid :: TID,            -- ^ thread id
      eventPayload_CommName :: ByteString -- ^ name of the application
   }
   -- Corresponds with the @mmap_event@ struct in @\<perf source\>\/util\/event.h@ (without the header).
   | MmapEvent {
      eventPayload_pid :: PID,                
      eventPayload_tid :: TID,                
      eventPayload_MmapStart :: Word64,       -- ^ start of memory range
      eventPayload_MmapLen :: Word64,         -- ^ size of memory range
      eventPayload_MmapPgoff :: Word64,       -- ^ page offset
      eventPayload_MmapFilename :: ByteString -- ^ binary file using this range
   }
   -- ForkEvent corresponds with the @fork_event@ struct in @\<perf source\>\/util\/event.h@ (without the header)
   | ForkEvent {
      eventPayload_pid :: PID,       
      eventPayload_ppid :: PID,      -- ^ parent proecess id
      eventPayload_tid :: TID,       
      eventPayload_ptid :: TID,      -- ^ parent thread id
      eventPayload_time :: TimeStamp -- ^ timestamp
   }
   -- Corresponds with the @exit_event@ struct in @\<perf source\>\/util\/event.h@ (without the header)
   | ExitEvent {
      eventPayload_pid :: PID,       
      eventPayload_ppid :: PID,      
      eventPayload_tid :: TID,       
      eventPayload_ptid :: TID,      
      eventPayload_time :: TimeStamp 
   }
   -- Corresponds with the @lost_event@ struct in @\<perf source\>\/util\/event.h@ (without the header)
   | LostEvent {
      eventPayload_id :: EventID,
      eventPayload_Lost :: Word64
   }
   -- Corresponds with the @read_event@ struct in @\<perf source\>\/util\/event.h@ (without the header)
   | ReadEvent {
      eventPayload_pid :: PID,
      eventPayload_tid :: TID,
      eventPayload_ReadValue :: Word64,
      eventPayload_ReadTimeEnabled :: Word64,
      eventPayload_ReadTimeRunning :: Word64,
      eventPayload_id :: EventID 
   }
   | SampleEvent {
      eventPayload_SampleIP :: Maybe Word64,       -- ^ Instruction pointer.
      eventPayload_SamplePID :: Maybe PID,         -- ^ Process ID.
      eventPayload_SampleTID :: Maybe TID,         -- ^ Thread ID.
      eventPayload_SampleTime :: Maybe TimeStamp,  -- ^ Timestamp.
      eventPayload_SampleAddr :: Maybe Word64,     --
      eventPayload_SampleID :: Maybe EventID,      -- ^ Event ID.
      eventPayload_SampleStreamID :: Maybe Word64, -- 
      eventPayload_SampleCPU :: Maybe Word32,      -- ^ CPU ID.
      eventPayload_SamplePeriod :: Maybe Word64    -- ^ Duration of sample.
   }
   -- ThrottleEvent and UnThrottleEvent are mentioned in
   -- @\<system include directory\>\/linux\/perf_event.h@
   -- but does not appear in @\<perf source\>\/util\/event.h@
   | ThrottleEvent {
      eventPayload_time :: TimeStamp, 
      eventPayload_id :: EventID,     
      eventPayload_stream_id :: Word64
   }
   | UnThrottleEvent {
      eventPayload_time :: TimeStamp,
      eventPayload_id :: EventID,
      eventPayload_stream_id :: Word64
   }
   | UnknownEvent -- ^ An unrecognised event was encountered.
   deriving (Show)

instance Pretty EventPayload where
   pretty ce@(CommEvent{}) =
      text "pid:" <+> pretty (eventPayload_pid ce) $$
      text "tid:" <+> pretty (eventPayload_tid ce) $$
      text "comm:" <+> pretty (eventPayload_CommName ce)
   pretty me@(MmapEvent{}) =
      text "pid:" <+> pretty (eventPayload_pid me) $$
      text "tid:" <+> pretty (eventPayload_tid me) $$
      text "start:" <+> pretty (eventPayload_MmapStart me) $$
      text "len:" <+> pretty (eventPayload_MmapLen me) $$
      text "pgoff:" <+> pretty (eventPayload_MmapPgoff me) $$
      text "filename:" <+> pretty (eventPayload_MmapFilename me)
   pretty fe@(ForkEvent{}) =
      text "pid:" <+> pretty (eventPayload_pid fe) $$
      text "ppid:" <+> pretty (eventPayload_ppid fe) $$
      text "tid:" <+> pretty (eventPayload_tid fe) $$
      text "ptid:" <+> pretty (eventPayload_ptid fe) $$
      text "time:" <+> pretty (eventPayload_time fe)
   pretty ee@(ExitEvent{}) =
      text "pid:" <+> pretty (eventPayload_pid ee) $$
      text "ppid:" <+> pretty (eventPayload_ppid ee) $$
      text "tid:" <+> pretty (eventPayload_tid ee) $$
      text "ptid:" <+> pretty (eventPayload_ptid ee) $$
      text "time:" <+> pretty (eventPayload_time ee)
   pretty le@(LostEvent {}) =
      text "id:" <+> pretty (eventPayload_id le) $$
      text "lost:" <+> pretty (eventPayload_Lost le)
   pretty se@(SampleEvent {}) =
      text "ip:" <+> pretty (eventPayload_SampleIP se) $$
      text "pid:" <+> pretty (eventPayload_SamplePID se) $$
      text "tid:" <+> pretty (eventPayload_SampleTID se) $$
      text "time:" <+> pretty (eventPayload_SampleTime se) $$
      text "addr:" <+> pretty (eventPayload_SampleAddr se) $$
      text "id:" <+> pretty (eventPayload_SampleID se) $$
      text "streamid:" <+> pretty (eventPayload_SampleStreamID se) $$
      text "cpu:" <+> pretty (eventPayload_SampleCPU se) $$
      text "period:" <+> pretty (eventPayload_SamplePeriod se)
   pretty te@(ThrottleEvent {}) =
      text "time:" <+> pretty (eventPayload_time te) $$
      text "id:" <+> pretty (eventPayload_id te) $$
      text "stream_id:" <+> pretty (eventPayload_stream_id te)
   pretty ue@(UnThrottleEvent {}) =
      text "time:" <+> pretty (eventPayload_time ue) $$
      text "id:" <+> pretty (eventPayload_id ue) $$
      text "stream_id:" <+> pretty (eventPayload_stream_id ue)
   pretty UnknownEvent = text "Unknown"
