{-# LANGUAGE RecordWildCards, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-----------------------------------------------------------------------------

import GHC.RTS.Events as GHC hiding (pid)
import Data.Word

import Profiling.Linux.Perf as Perf
   ( readPerfData, PID (..), TID (..), PerfTypeID (..), PerfFileContents
   , EventPayload (..), Event (..), EventTypeID (..), TraceEventType (..)
   , FileAttr (..), EventAttr (..))
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Data.Word (Word32)
import Data.Map (toList)
import Data.Set as Set (fromList, Set, member, empty, insert, toList)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Data.Map as Map hiding (mapMaybe, map, filter, null, foldr)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (sortBy)

-- XXX this should move to to-eventlog (and perhaps even be deforested)
data PerfEvent =
   PerfEvent
   { perfEvent_identity :: Word64   -- sample ID
   , perfEvent_pid :: PID           -- process ID
   , perfEvent_tid :: TID           -- thread ID
   , perfEvent_timestamp :: Word64  -- timestamp in nanoseconds since some arbitrary point
                                    -- in time, probably system boot.
   , perfEvent_type :: PerfTypeID   -- what kind of sample is it? hardware, software, tracepoint etc?
   , perfEvent_typeName :: String   -- the name of the event type
   }

-- Compare two events based on their timestamp.
compareSamplePayload :: EventPayload -> EventPayload -> Ordering
compareSamplePayload e1 e2 = compare (getEventTime e1) (getEventTime e2)

-- Get the timestamp of an event if it has one, otherwise
-- set it to 0 (for the purposes of sorting them).
getEventTime :: EventPayload -> Word64
getEventTime e@(SampleEvent {}) = maybe 0 id $ se_time e
getEventTime e@(ForkEvent {}) = fe_time e
getEventTime e@(ExitEvent {}) = ee_time e
getEventTime e@(ThrottleEvent {}) = te_time e
getEventTime e@(UnThrottleEvent {}) = ue_time e
getEventTime other = 0

-- convert perf event payloads into the PerfEvent representation.
-- the payload must be a sample which has a process ID, thread ID,
-- timestamp and identity. Any other payload is skipped.
mkPerfEvent :: PerfEventTypeMap -> EventPayload -> Maybe PerfEvent
mkPerfEvent eventTypeMap (se@SampleEvent {})
   | Just perfEvent_pid <- se_pid se,
     Just perfEvent_tid <- se_tid se,
     Just perfEvent_timestamp <- se_time se,
     Just perfEvent_identity <- se_id se,
     Just (perfEvent_typeName, perfEvent_type)
        <- Map.lookup perfEvent_identity eventTypeMap
        = Just $ PerfEvent {..}
   | otherwise = Nothing
mkPerfEvent _eventTypeMap _otherEvent = Nothing

-- Mapping from event ID to event name
type PerfEventTypeMap = Map Word64 (String, PerfTypeID)

-- return a list of perf events in timestamp order, and a mapping from
-- event ID to the event name
perfTrace :: FilePath -> IO [PerfEvent]
perfTrace file = makeTrace `fmap` readPerfData file

makeTrace :: PerfFileContents -> [PerfEvent]
makeTrace (header, attrs, idss, types, events) =
   mapMaybe (mkPerfEvent eventTypeMap) $
      sortBy compareSamplePayload $
      map ev_payload events
   where
   -- mapping from type id to type name
   typesMap :: Map EventTypeID String
   typesMap = Map.fromList [(te_event_id t, unpack $ te_name t) | t <- types]
   -- mapping from event id to type name
   eventTypeMap :: PerfEventTypeMap
   eventTypeMap = makeAttrsMap $ zip (map fa_attr attrs) idss
   makeAttrsMap :: [(EventAttr, [Word64])] -> PerfEventTypeMap
   makeAttrsMap = foldr idsToName Map.empty
   idsToName :: (EventAttr, [Word64]) -> PerfEventTypeMap -> PerfEventTypeMap
   idsToName (attr, ids) result =
      case Map.lookup eventID typesMap of
         -- We don't have a type name for this particular event.
         -- This shouldn't happen in a well formatted perf data file, 
         -- but we ignore it for the moment.
         Nothing -> result
         -- Update the event type map, mapping each event id to the name.
         Just name -> foldr (flip Map.insert (name, eventType)) result ids
      where
      eventID = ea_config attr   
      eventType = ea_type attr

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

perfToGHC :: PID -> [PerfEvent] -> [GHC.Event]
perfToGHC pid perfEvents =
   typeEvents ++ reverse events 
   where
   -- we fold over the list of perf events and collect a set of
   -- event types and a list of ghc events
   (typeEventSet, events) = Prelude.foldl perfToGHCWorker (Set.empty, []) perfEvents
   -- convert the set of perf type infos into a list of events
   typeEvents :: [GHC.Event]
   typeEvents = mkTypeEvents $ Set.toList typeEventSet
   mkTypeEvents :: [(String, Word32)] -> [GHC.Event]
   mkTypeEvents = map (\(name, id) -> GHC.Event 0 $ PerfName id name)
   -- extract a new type event and ghc event from the next perf event
   -- and update the state
   perfToGHCWorker :: (Set PerfTypeInfo, [GHC.Event]) -> PerfEvent -> (Set PerfTypeInfo, [GHC.Event])
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
      newEvent = GHC.Event perfTimestamp newEventBody
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

eventLog :: [GHC.Event] -> EventLog
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
