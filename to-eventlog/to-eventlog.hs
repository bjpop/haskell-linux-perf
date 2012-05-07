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
import Data.Char (isDigit)

main :: IO ()
main = do
  -- read and parse the command line arguments
  args <- getArgs
  case args of
    [pidStr, inF, outF] ->
       case parsePID pidStr of
          Nothing -> die "Invalid PID argument, must be an integer"
          Just pid -> do
             -- read the linux perf data file
             perfEvents <- perfTrace inF
             -- convert the perf data to ghc events
             let perfEventlog = perfToEventlog pid perfEvents
             -- write the ghc events out to file
             writeEventLogToFile outF perfEventlog
    _ -> die "Syntax: to-eventlog [pid perf_file eventlog_file]"

-- exit the program with an error message
die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

-- parse the command line input for a valid process ID
parsePID :: String -> Maybe PID
parsePID str
   | length str > 0 && all isDigit str = Just $ PID $ read str
   | otherwise = Nothing

-- convert a list of linux perf events into a ghc event log
perfToEventlog :: PID -> [PerfEvent] -> EventLog
perfToEventlog pid events =
   eventLog $ perfToGHC pid events

-- Perf type name and identity
type PerfTypeInfo = (String, Word32)

perfToGHC :: PID -> [PerfEvent] -> [Event]
perfToGHC pid perfEvents =
   typeEvents ++ reverse events 
   where
   -- we fold over the list of perf events and collect a set of
   -- event types and a list of ghc events
   (typeEventSet, events) = foldl perfToGHCWorker (Set.empty, []) perfEvents
   -- convert the set of perf type infos into a list of events
   typeEvents :: [Event]
   typeEvents = mkTypeEvents $ Set.toList typeEventSet
   mkTypeEvents :: [(String, Word32)] -> [Event]
   mkTypeEvents = map (\(name, id) -> Event 0 $ PerfName id name)
   -- extract a new type event and ghc event from the next perf event
   -- and update the state
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
      -- collect the type of event and also the ghc event information
      | otherwise = (newNameSet, newEvent : events)
      where
      newNameSet = Set.insert newNameInfo nameSet
      newNameInfo = (perfTypeName, ghcID)
      newEvent = Event perfTimestamp newEventBody
      ghcTID = fromIntegral $ tid perfTID
      ghcID = fromIntegral perfID 
      -- generate the appropriate ghc event
      newEventBody
         -- it is a tracepoint
         | perfType == PerfTypeTracePoint = PerfTracepoint ghcID ghcTID
         -- it is some kind of counter
         | otherwise = PerfCounter ghcID 0 -- XXX count is incorrect

-- test if a perf event is for a given process ID
eventPID :: PID -> PerfEvent -> Bool
eventPID pidTarget event = pidTarget == perfEvent_pid event

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
