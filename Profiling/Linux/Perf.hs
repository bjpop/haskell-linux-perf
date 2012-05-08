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

module Profiling.Linux.Perf
   ( module Profiling.Linux.Perf.Parse
   , PerfFileContents
   , OutputStyle (..)
   , readAndDisplay
   , readPerfData
   , display
   , perfTrace
   , PerfEvent (..)
   ) where

import Profiling.Linux.Perf.Parse
   ( FileHeader (..), FileAttr (..), TraceEventType (..), Event (..),  EventPayload (..), EventHeader (..)
   , EventAttr (..), readHeader, readAttributes, readAttributeIDs, readEventTypes, readEvent
   , EventAttrFlag (..), testEventAttrFlag, PID (..), TID (..), EventTypeID (..), PerfTypeID (..) )
import Profiling.Linux.Perf.Pretty ( pretty )
import Text.PrettyPrint as Pretty
   ( render, Doc, empty, text, (<+>), (<>), vcat, ($$), int, hsep )
import Data.Word (Word64, Word32)
import Data.List (intersperse, sortBy)
import Data.Map as Map hiding (mapMaybe, map, filter, null, foldr)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Bits (testBit)
import System.IO (openFile, IOMode(ReadMode), Handle)
import Data.Maybe (mapMaybe)

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
   typesMap = fromList [(te_event_id t, unpack $ te_name t) | t <- types]
   -- typesIDsNames = map (\t -> (te_event_id t, unpack $ te_name t)) types
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

-- style to use for printing the event data
data OutputStyle = Dump

-- bit position of sample_id_all in the flags part of event_attr
sampleIdAllPos :: Int
sampleIdAllPos = 18

-- The various parts of the perf.data file collected together
type PerfFileContents = (FileHeader, [FileAttr], [[Word64]], [TraceEventType], [Event])

-- read the contents of the perf.data file and render it
-- on stdout in a specified style.
readAndDisplay :: OutputStyle -> FilePath -> IO ()
readAndDisplay style file = display style =<< readPerfData file

-- Read and parse the perf.data file into its constituent components
readPerfData :: FilePath -> IO PerfFileContents
readPerfData file = do
   h <- openFile file ReadMode
   header <- readHeader h
   attrs <- readAttributes h header
   idss <- mapM (readAttributeIDs h) attrs
   types <- readEventTypes h header
   -- See: samplingType in perffile/session.c and the way it is set in the CERN readperf code.
   -- They also assume there is just one sampleType.
   let attrTypeInfo = getAttrInfo attrs
       (sampleType, sampleIdAll) =
          case attrTypeInfo of
             []  -> (0, False)
             x:_ -> x
       dataOffset = fh_data_offset header
       maxOffset = fh_data_size header + dataOffset
   events <- readEvents h maxOffset dataOffset sampleType
   return (header, attrs, idss, types, events)

-- Render the components of the perf.data file under the specified style
display :: OutputStyle -> PerfFileContents -> IO ()
display style contents = do
   putStrLn $ render $ case style of
      Dump ->  dumper contents

-- Get the Sample Type and test the sample_id_all bit in the flags field
getAttrInfo :: [FileAttr] -> [(Word64, Bool)]
getAttrInfo = map getSampleTypeAndIdAll
   where
   getSampleTypeAndIdAll :: FileAttr -> (Word64, Bool)
   getSampleTypeAndIdAll fattr
      = (ea_sample_type attr, testEventAttrFlag (ea_flags attr) SampleIdAll)
      where
      attr = fa_attr $ fattr

-- read the events from file and return them in the order that they appear
-- (not sorted on timestamp).
readEvents :: Handle -> Word64 -> Word64 -> Word64 -> IO [Event]
readEvents h maxOffset offset sampleType =
   readWorker offset []
   where
   readWorker :: Word64 -> [Event] -> IO [Event]
   readWorker offset acc
      | offset >= maxOffset = return $ reverse acc
      | otherwise = do
           event <- readEvent h offset sampleType
           let size = eh_size $ ev_header event
               nextOffset = offset + fromIntegral size
           readWorker nextOffset (event:acc)

-- Dump the events in a sequence, showing all their internal values.
dumper :: PerfFileContents -> Doc
dumper (header, attrs, idss, types, events) =
   vcat $ intersperse separator $
      [ text "Perf File Header:"
      , pretty header
      , text "Perf File Attributes:"
      , vcat $ intersperse separator $
               map prettyAttrAndIds $ zip attrs idss
      , text "Trace Event Types:"
      , vcat $ map pretty types
      , text "Events:"
      ] ++ map pretty events
   where
   prettyAttrAndIds (attr, ids) =
      pretty attr $$ (text "ids:" <+> (hsep $ map pretty ids))
   separator :: Doc
   separator = text $ replicate 40 '-'

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
