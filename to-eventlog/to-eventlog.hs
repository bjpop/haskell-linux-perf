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

import Profiling.Linux.Perf (PerfEvent (..), perfTrace, PID (..), TID (..), PerfTypeID (..))
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Data.Word (Word32)
import Data.Map (toList)
import Data.Set as Set (fromList, Set, member, empty, insert, toList)
import Data.Maybe (mapMaybe)

die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [pid, inF, outF] -> do
       -- read the linux perf data file
       perfEvents <- perfTrace inF
       -- convert the perf data to ghc events
       let perfEventlog = perfToEventlog (PID $ read pid) perfEvents
       -- write the ghc events out to file
       writeEventLogToFile outF perfEventlog
    _ -> die "Syntax: to-eventlog [pid perf_file eventlog_file]"

perfToEventlog :: PID -> [PerfEvent] -> EventLog
perfToEventlog pid events =
   eventLog $ perfToGHC pid events

type PerfTypeInfo = (String, Word32)

perfToGHC :: PID -> [PerfEvent] -> [Event]
perfToGHC pid perfEvents =
   typeEvents ++ reverse events 
   where
   typeEvents = mkTypeEvents $ Set.toList typeEventSet
   mkTypeEvents :: [(String, Word32)] -> [Event]
   mkTypeEvents = map (\(name, id) -> Event 0 $ PerfName id name)
   (typeEventSet, events) = foldl perfToGHCWorker (Set.empty, []) perfEvents
   perfToGHCWorker :: (Set PerfTypeInfo, [Event]) -> PerfEvent -> (Set PerfTypeInfo, [Event])
   perfToGHCWorker (nameSet, events)
                   (PerfEvent
                    { perfEvent_identity = perfID
                    , perfEvent_pid = perfPID
                    , perfEvent_tid = perfTID
                    , perfEvent_timestamp = perfTimestamp
                    , perfEvent_type = perfType
                    , perfEvent_typeName = perfTypeName
                    })
      -- test if the event belongs to the process of interest
      | perfPID /= pid = (nameSet, events)
      | otherwise = (newNameSet, newEvent : events)
      where
      newNameSet = Set.insert newNameInfo nameSet
      newNameInfo = (perfTypeName, ghcID)
      newEvent = Event perfTimestamp newEventBody
      ghcTID = fromIntegral $ tid perfTID
      ghcID = fromIntegral perfID 
      newEventBody
         | perfType == PerfTypeTracePoint = PerfTracepoint ghcID ghcTID
         | otherwise = PerfCounter ghcID 0 -- XXX count is incorrect

-- test if a perf event is for a given process ID
eventPID :: PID -> PerfEvent -> Bool
eventPID pidTarget event = pidTarget == perfEvent_pid event

{-
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
-}

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
