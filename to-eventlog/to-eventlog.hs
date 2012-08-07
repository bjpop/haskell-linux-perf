{-# LANGUAGE RecordWildCards, PatternGuards, NamedFieldPuns #-}
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

import qualified GHC.RTS.Events as GHC
import Profiling.Linux.Perf as Perf
   ( readPerfData, makeTypeMap, sortEventsOnTime, TypeMap, TypeInfo (..) )
import Profiling.Linux.Perf.Types as Perf
   ( TID (..), EventSource (..), EventPayload (..), Event (..), EventID
   , EventTypeID (..), TraceEventType (..) , FileAttr (..), EventAttr (..)
   , TimeStamp (..), PerfData (..) )
import Control.Monad (when)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Data.Word (Word64, Word32, Word16)
import Data.Map (toList)
import Data.Set as Set (fromList, Set, member, empty, insert, toList)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Char (isDigit)
import Data.Map as Map hiding (mapMaybe, map, filter, null)
import Data.List as List (sortBy, foldl', find)

main :: IO ()
main = do
  -- read and parse the command line arguments
  args <- getArgs
  case args of
    [inFile, outFile] -> do
      -- read the linux perf data file
      perfFileContents <- readPerfData inFile
      -- convert the perf data to ghc events
      let startT = getStartTimestamp perfFileContents
          perfEventlog = perfToEventlog startT perfFileContents
      -- debug
      print startT
      -- write the ghc events out to file
      GHC.writeEventLogToFile outFile perfEventlog
    _ -> die "Syntax: to-eventlog [perf_file eventlog_file]"

-- Gets the timestamp of the first occurence of a perf sample event
-- with markerEventId.
getStartTimestamp :: PerfData -> Maybe Word64
getStartTimestamp (PerfData header attrs idss types events) =
  let isMarkerFileAttr (FileAttr {fa_attr = EventAttr {ea_config}}, _) =
        ea_config == markerEventId
      eids :: [EventID]
      eids = concat $ map snd $ filter isMarkerFileAttr $ zip attrs idss
      -- Sort the events by timestamp order to get the first occurence.
      sortedEventPayloads :: [EventPayload]
      sortedEventPayloads = map ev_payload $ sortEventsOnTime events
      isMarkerPayload SampleEvent{eventPayload_SampleID = Just si} =
        si `elem` eids
      isMarkerPayload _ = False
  in do
    marker <- find isMarkerPayload sortedEventPayloads
    stamp <- eventPayload_SampleTime marker
    return $ timeStamp stamp

--   id of the sys_exit_nanosleep syscall
-- TODO: hardcoded for now, take it from the header instead
markerEventId :: EventTypeID
markerEventId = EventTypeID 273

-- exit the program with an error message
die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

-- Convert linux perf event data into a ghc event log.
perfToEventlog :: Maybe Word64 -> PerfData -> GHC.EventLog
perfToEventlog mstart perfData =
   eventLog $ perfToGHC mstart (makeTypeMap perfData) sortedEventPayloads
   where
   -- sort the events by timestamp order
   sortedEventPayloads :: [EventPayload]
   sortedEventPayloads =
      map ev_payload $ sortEventsOnTime $ perfData_events perfData
   eventLog :: [GHC.Event] -> GHC.EventLog
   eventLog events = GHC.EventLog (GHC.Header perfEventlogHeader)
                                  (GHC.Data events)

type TypeNameAndID = (String, Word32 {- PerfEventTypeNum -} )
type TypeSet = Set TypeNameAndID
type EventState = (TypeSet, [GHC.Event])

perfToGHC :: Maybe Word64   -- initial timestamp
          -> TypeMap        -- mapping from event ID to event type info
          -> [EventPayload] -- perf events in sorted time order
          -> [GHC.Event]    -- ghc event log
perfToGHC mstart typeMap perfEvents =
   typeEvents ++ reverse ghcEvents
   where
   typeEvents :: [GHC.Event]
   typeEvents = mkTypeEvents $ Set.toList typeEventSet
   -- we fold over the list of perf events and collect a set of
   -- event types and a list of ghc events
   (typeEventSet, ghcEvents) = List.foldl' perfToGHCWorker (Set.empty, []) perfEvents
   -- convert the set of perf type infos into a list of events
   mkTypeEvents :: [TypeNameAndID] -> [GHC.Event]
   mkTypeEvents = map (\(name, id) -> GHC.Event 0 $ GHC.PerfName id name)
   -- extract a new type event and ghc event from the next perf event
   -- and update the state
   perfToGHCWorker :: EventState -> EventPayload -> EventState
   perfToGHCWorker state@(typeSet, events) SampleEvent{..} =
      fromMaybe state $ do
         eventTID <- eventPayload_SampleTID
         eventID <- eventPayload_SampleID
         absoluteTime <- fmap timeStamp eventPayload_SampleTime
         let relativeTime = absoluteTime - (fromMaybe 0 mstart)
         -- lookup the event type for this event
         TypeInfo typeName typeSource typeID <- Map.lookup eventID typeMap
         let newTypeSet = Set.insert (typeName, ghcTypeID) typeSet
             ghcTID = GHC.KernelThreadId $ fromIntegral $ tid eventTID
             ghcTypeID = fromIntegral $ eventTypeID typeID
             -- generate the appropriate ghc event
             newEvent = GHC.Event relativeTime newEventBody
             newEventBody
                -- it is a tracepoint
                | typeSource == PerfTypeTracePoint =
                    GHC.PerfTracepoint ghcTypeID ghcTID
                -- it is some kind of counter
                | otherwise =
                    let eventPeriod = fromMaybe 0 eventPayload_SamplePeriod
                    in GHC.PerfCounter ghcTypeID ghcTID eventPeriod
         seq newTypeSet $ return (newTypeSet, newEvent:events)
   -- skip any other type of event which is not a SampleEvent
   perfToGHCWorker eventState otherEvent = eventState

perfEventlogHeader :: [GHC.EventType]
perfEventlogHeader =
  [ GHC.EventType GHC.nEVENT_PERF_NAME "perf event name" Nothing
  , GHC.EventType GHC.nEVENT_PERF_COUNTER "perf event counter"
                  (Just $ GHC.sz_perf_num + GHC.sz_kernel_tid + 8)
  , GHC.EventType GHC.nEVENT_PERF_TRACEPOINT "perf event tracepoint"
                  (Just $ GHC.sz_perf_num + GHC.sz_kernel_tid)
  ]
