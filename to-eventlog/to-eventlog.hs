-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-----------------------------------------------------------------------------

import GHC.RTS.Events
import Data.Word

main :: IO ()
main = do
  writeEventLogToFile "test.eventlog" test
  putStr $ ppEventLog test

test :: EventLog
test = eventLog $
  [ Event 0 (PerfName 0 "L2 cache misses")
  , Event 1000 (PerfCounter 0 1)
  , Event 1100 (PerfCounter 0 2)
  , Event 2000 (PerfName 1 "kill")
  , Event 2100 (PerfCounter 0 3)
  , Event 2200 (PerfTracepoint 1 0)
  ]

eventLog :: [Event] -> EventLog
eventLog events = EventLog (Header testEventTypes) (Data events)

perfName :: Word16
perfName = 140

perfCounter :: Word16
perfCounter = 141

perfTracepoint :: Word16
perfTracepoint = 142

testEventTypes :: [EventType]
testEventTypes =
  [ EventType perfName "perf event name" Nothing
  , EventType perfCounter "perf event counter" (Just $ sz_perf_num + 8)
  , EventType perfTracepoint "perf event tracepoint"
      (Just $ sz_perf_num + sz_tid)
  ]

type EventTypeSize = Word16

sz_perf_num :: EventTypeSize
sz_perf_num = 4

sz_tid :: EventTypeSize
sz_tid  = 4
