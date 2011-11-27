module Profiling.Linux.Perf.Types {- (
  )-} where

import Data.Word

-- -----------------------------------------------------------------------------
-- Event data types

data Event = Event { ev_type    :: EventType,
                     ev_cpumode :: EventCPUMode,
                     ev_size    :: Word16,
                     ev_data    :: [Word8] }

data EventType -- perf_event_header->type
  = EventType_0            -- unused
  | PERF_RECORD_MMAP       -- 1
  | PERF_RECORD_LOST       -- 2
  | PERF_RECORD_COMM       -- 3
  | PERF_RECORD_EXIT       -- 4
  | PERF_RECORD_THROTTLE   -- 5
  | PERF_RECORD_UNTHROTTLE -- 6
  | PERF_RECORD_FORK       -- 7
  | PERF_RECORD_READ       -- 8
  | PERF_RECORD_SAMPLE     -- 9

data EventCPUMode -- a bitfield in perf_event_header->misc
  = PERF_RECORD_CPUMODE_UNKNOWN -- 0
  | PERF_RECORD_MISC_KERNEL     -- 1
  | PERF_RECORD_MISC_USER       -- 2
  | PERF_RECORD_MISC_HYPERVISOR -- 3

