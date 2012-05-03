-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-----------------------------------------------------------------------------

import GHC.RTS.Events hiding (pid)
import Data.Word

import Profiling.Linux.Perf (PerfEvent (..), perfTrace, PerfEventTypeMap)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Data.Word (Word32)
import Data.Map (toList)
import Data.Set as Set (fromList, Set, member)
import Data.Maybe (mapMaybe)

-- process identity
type PID = Word32

die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [pid, inF, outF] -> do
       -- read the linux perf data file
       (perfEventTypeMap, perfEvents) <- perfTrace inF
       -- convert the perf data to ghc events
       let perfEventlog = perfToEventlog (read pid) perfEventTypeMap perfEvents
       -- write the ghc events out to file
       writeEventLogToFile outF perfEventlog
    _ -> die "Syntax: to-eventlog [pid perf_file eventlog_file]"

perfToEventlog :: PID -> PerfEventTypeMap -> [PerfEvent] -> EventLog
perfToEventlog pid typeMap events =
   eventLog (perfEventTypes ++ perfEvents)
   where
   perfEventTypes = typeMapToEvents eventIDs typeMap
   perfEvents = map perfToGHC $ filter (eventPID pid) events
   eventIDs :: Set Word64
   eventIDs = Set.fromList $ map identity events

-- test if a perf event is for a given process ID
eventPID :: PID -> PerfEvent -> Bool
eventPID pidTarget event = pidTarget == pid event

-- only collect only the types that are referred to by events in
-- the process we are interested in (not all types).
typeMapToEvents :: Set Word64 -> PerfEventTypeMap -> [Event]
typeMapToEvents eventTypes typeMap =
   mapMaybe toPerfName $ toList typeMap
   where
   toPerfName :: (Word64, String) -> Maybe Event
   toPerfName (identity, eventName)
      | identity `Set.member` eventTypes = 
           Just $ Event 0 $
                  PerfName
                  { perfNum = fromIntegral identity
                  , name = eventName }
      | otherwise = Nothing

-- XXX we don't currently generate PerfCounter events, need to find out if
-- perf records whether an event is a counter or a tracepoint.
perfToGHC :: PerfEvent -> Event
perfToGHC e@(PerfSample {}) =
   Event (timestamp e) $
         PerfTracepoint
         { perfNum = fromIntegral $ identity e
         , thread = tid e }

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

{-
test :: EventLog
test = eventLog $
  [ Event 0 (PerfName 0 "L2 cache misses")
  , Event 1000 (PerfCounter 0 1)
  , Event 1100 (PerfCounter 0 2)
  , Event 2000 (PerfName 1 "kill")
  , Event 2100 (PerfCounter 0 3)
  , Event 2200 (PerfTracepoint 1 0)
  ]
-}
