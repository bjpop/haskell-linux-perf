{-# LANGUAGE RecordWildCards, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Convert linux perf data into a GHC event log.
--
-----------------------------------------------------------------------------

import GHC.RTS.Events as GHC hiding (pid)
import Profiling.Linux.Perf as Perf
   ( readPerfData, PID (..), TID (..), EventSource (..), PerfData (..)
   , EventPayload (..), Event (..), EventTypeID (..), TraceEventType (..)
   , FileAttr (..), EventAttr (..), makeTypeMap, sortEventsOnTime
   , TypeMap, TypeInfo (..), TimeStamp (..) )
import Control.Monad (when)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Data.Word (Word32, Word16)
import Data.Map (toList)
import Data.Set as Set (fromList, Set, member, empty, insert, toList)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.Map as Map hiding (mapMaybe, map, filter, null, foldr)
import Data.List as List (sortBy, foldl')

main :: IO ()
main = do
  -- read and parse the command line arguments
  args <- getArgs
  case args of
    [pidStr, inFile, outFile] ->
       case parsePID pidStr of
          Nothing -> die "Invalid PID argument, must be an integer"
          Just pid -> do
             -- read the linux perf data file
             perfFileContents <- readPerfData inFile
             -- convert the perf data to ghc events
             let perfEventlog = perfToEventlog pid perfFileContents
             -- write the ghc events out to file
             writeEventLogToFile outFile perfEventlog
    _ -> die "Syntax: to-eventlog [pid perf_file eventlog_file]"

-- exit the program with an error message
die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

-- parse the command line input for a valid process ID
parsePID :: String -> Maybe PID
parsePID str
   | length str > 0 && all isDigit str = Just $ PID $ read str
   | otherwise = Nothing

-- Convert linux perf event data into a ghc event log.
-- The PID identifies the process that we are tracing.
perfToEventlog :: PID -> PerfData -> EventLog
perfToEventlog pid perfData =
   eventLog $ perfToGHC pid (makeTypeMap perfData) sortedEventPayloads
   where
   -- sort the events by timestamp order
   sortedEventPayloads :: [EventPayload]
   sortedEventPayloads = 
      map ev_payload $ sortEventsOnTime $ perfData_events perfData
   eventLog :: [GHC.Event] -> EventLog
   eventLog events = EventLog (Header testEventTypes) (Data events)

type TypeNameAndID = (String, Word32 {- PerfEventTypeNum -} )
type TypeSet = Set TypeNameAndID
type EventState = (TypeSet, [GHC.Event])

perfToGHC :: PID            -- process ID
          -> TypeMap        -- mapping from event ID to event type info
          -> [EventPayload] -- perf events in sorted time order
          -> [GHC.Event]    -- ghc event log
perfToGHC targetPID typeMap perfEvents =
   typeEvents ++ reverse ghcEvents 
   where
   typeEvents :: [GHC.Event]
   typeEvents = mkTypeEvents $ Set.toList typeEventSet
   -- we fold over the list of perf events and collect a set of
   -- event types and a list of ghc events
   (typeEventSet, ghcEvents) = List.foldl' perfToGHCWorker (Set.empty, []) perfEvents
   -- convert the set of perf type infos into a list of events
   mkTypeEvents :: [TypeNameAndID] -> [GHC.Event]
   mkTypeEvents = map (\(name, id) -> GHC.Event 0 $ PerfName id name)
   -- extract a new type event and ghc event from the next perf event
   -- and update the state
   perfToGHCWorker :: EventState -> EventPayload -> EventState 
   perfToGHCWorker state@(typeSet, events) se@(SampleEvent {}) = 
      maybe state id $ do
         eventPID <- eventPayload_SamplePID se
         eventTID <- eventPayload_SampleTID se
         eventID <- eventPayload_SampleID se 
         eventTime <- eventPayload_SampleTime se
         if (eventPID == targetPID) then do
            -- lookup the event type for this event
            TypeInfo typeName typeSource typeID <- Map.lookup eventID typeMap
            let newTypeSet = Set.insert (typeName, ghcTypeID) typeSet 
                ghcTID = fromIntegral $ tid eventTID
                ghcTypeID = fromIntegral $ eventTypeID typeID
                -- generate the appropriate ghc event
                newEvent = GHC.Event (timeStamp eventTime) newEventBody
                newEventBody
                   -- it is a tracepoint
                   | typeSource == PerfTypeTracePoint = PerfTracepoint ghcTypeID ghcTID
                   -- it is some kind of counter
                   | otherwise = PerfCounter ghcTypeID 0 -- XXX count is incorrect
            seq newTypeSet $ return (newTypeSet, newEvent:events)
            else Nothing
   -- skip any other type of event which is not a SampleEvent
   perfToGHCWorker eventState otherEvent = eventState

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
