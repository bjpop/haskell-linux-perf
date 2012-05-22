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
   , PerfData (..)
   , TypeMap
   , TypeInfo (..)
   , OutputStyle (..)
   , readAndDisplay
   , readPerfData
   , display
   , makeTypeMap
   , sortEventsOnTime
   ) where

import Profiling.Linux.Perf.Parse
   ( FileHeader (..), FileAttr (..), TraceEventType (..), Event (..),  EventPayload (..), EventHeader (..)
   , EventAttr (..), readHeader, readAttributes, readAttributeIDs, readEventTypes, readEvent
   , EventAttrFlag (..), testEventAttrFlag, PID (..), TID (..), EventTypeID (..), EventSource (..)
   , EventID (..) )
import Profiling.Linux.Perf.Pretty ( pretty )
import Text.PrettyPrint as Pretty
   ( render, Doc, empty, text, (<+>), (<>), vcat, ($$), int, hsep )
import Data.Word (Word64, Word32)
import Data.List as List (intersperse, sortBy, foldl')
import Data.Map as Map hiding (mapMaybe, map, filter, null, foldr)
import Data.Bits (testBit)
import System.IO (openFile, IOMode(ReadMode), Handle)
import Data.Maybe (mapMaybe)
import Data.ByteString.Lazy.Char8 (unpack)

-- associate events with their event types
type TypeMap = Map EventID TypeInfo

data TypeInfo =
   TypeInfo
   { typeInfo_name :: String        -- name of event/counter source
   , typeInfo_source :: EventSource -- kind of event/counter source
   , typeInfo_id :: EventTypeID     -- magic unique number of this type of event/counter
   }

sortEventsOnTime :: [Event] -> [Event]
sortEventsOnTime =
   sortBy compareEventTime
   where
   -- Compare two events based on their timestamp.
   compareEventTime :: Event -> Event -> Ordering
   compareEventTime e1 e2 =
      compare (getEventTime $ ev_payload e1) (getEventTime $ ev_payload e2) 
   -- Get the timestamp of an event if it has one, otherwise
   -- set it to 0 (for the purposes of sorting them).
   getEventTime :: EventPayload -> Word64
   getEventTime e@(SampleEvent {}) = maybe 0 id $ se_time e
   getEventTime e@(ForkEvent {}) = fe_time e
   getEventTime e@(ExitEvent {}) = ee_time e
   getEventTime e@(ThrottleEvent {}) = te_time e
   getEventTime e@(UnThrottleEvent {}) = ue_time e
   getEventTime other = 0 

makeTypeMap :: PerfData -> TypeMap
makeTypeMap perfData =
   List.foldl' idsToInfo Map.empty attrsIDs
   where
   idss :: [[EventID]]
   idss = perfData_idss perfData
   types :: [TraceEventType]
   types = perfData_types perfData
   attrs :: [EventAttr]
   attrs = map fa_attr $ perfData_attrs perfData
   -- pair attributes with their corresponding event IDs
   attrsIDs :: [(EventAttr, EventID)]
   attrsIDs = [(attribute, id) | (attribute, idents) <- zip attrs idss, id <- idents]
   -- mapping from event type id to name
   eventTypeToName :: Map EventTypeID String
   eventTypeToName = Map.fromList $ map (\t -> (te_event_id t, unpack $ te_name t)) types
   -- update the type map with a new entry for a given event type
   idsToInfo :: TypeMap -> (EventAttr, EventID) -> TypeMap
   idsToInfo acc (attr, eventID) =
      case Map.lookup typeID eventTypeToName of
         -- We don't have a type name for this particular event.
         -- This shouldn't happen in a well formatted perf data file, 
         -- but we ignore it for the moment.
         Nothing -> acc
         -- Update the event type map, mapping each event id to the name.
         Just typeName ->
            Map.insert eventID (TypeInfo typeName typeSource typeID) acc 
      where
      typeSource = ea_type attr 
      typeID = ea_config attr   

-- The various parts of the perf.data file collected together
data PerfData =
   PerfData
   { perfData_fileHeader :: FileHeader
   , perfData_attrs :: [FileAttr]
   , perfData_idss :: [[EventID]]
   , perfData_types :: [TraceEventType]
   , perfData_events :: [Event] 
   }

-- read the contents of the perf.data file and render it
-- on stdout in a specified style.
readAndDisplay :: OutputStyle -> FilePath -> IO ()
readAndDisplay style file = display style =<< readPerfData file

-- Read and parse the perf.data file into its constituent components
readPerfData :: FilePath -> IO PerfData
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
   return $ PerfData header attrs idss types events

-- style to use for printing the event data
data OutputStyle = Dump

-- Render the components of the perf.data file under the specified style
display :: OutputStyle -> PerfData -> IO ()
display style contents = do
   putStrLn $ render $ case style of
      Dump -> dumper contents

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
dumper :: PerfData -> Doc
dumper (PerfData header attrs idss types events) =
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
