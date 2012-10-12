{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A higher-level interface to the perf data file parsing code.
--
-- Below is an example program which reads and parses a perf.data file and then
-- dumps the contents to standard output:
--
-- @
--module Main where
--
--import Profiling.Linux.Perf (readAndDisplay, OutputStyle (..))
--import System.Environment (getArgs)
--
--main :: IO ()
--main = do
--  args <- getArgs
--  case args of
--     [] -> return ()
--     (file:_) -> readAndDisplay Dump file
-- @

-----------------------------------------------------------------------------

module Profiling.Linux.Perf
   ( -- * Data types
     TypeMap
   , TypeInfo (..)
   , OutputStyle (..)
     -- * Functions
   , readAndDisplay
   , readPerfData
   , display
   , makeTypeMap
   , sortEventsOnTime
   ) where

import Profiling.Linux.Perf.Parse
   ( readHeader, readAttributes, readAttributeIDs, readEventTypes, readEvent )
import Profiling.Linux.Perf.Types
   ( FileHeader (..), FileAttr (..), TraceEventType (..), Event (..)
   , EventPayload (..), EventHeader (..) , EventAttr (..), EventAttrFlag (..)
   , testEventAttrFlag, PID (..), TID (..), EventTypeID (..), EventSource (..)
   , EventID (..), TimeStamp (..), SampleTypeBitMap (..), ByteCount64 (..)
   , PerfData (..) )
import Profiling.Linux.Perf.Pretty ( pretty )
import Text.PrettyPrint as Pretty
   ( render, Doc, empty, text, (<+>), (<>), vcat, ($$), int, hsep, hcat )
import Data.List as List (intersperse, sortBy, foldl')
import Data.Map as Map hiding (mapMaybe, map, filter, null)
import System.IO (openFile, IOMode(ReadMode), Handle)
import Data.Maybe (mapMaybe)
import Data.ByteString.Lazy.Char8 (unpack)

-- | Associate events with their event types.
-- Events are (usually) tagged with an "EventID". Many events can share the same
-- "EventID". Each "EventID" is associated with exactly one event type, which includes
-- the name of the event, an "EventSource" and an "EventTypeID"
type TypeMap = Map EventID TypeInfo

-- | Type information for of event.
data TypeInfo =
   TypeInfo
   { typeInfo_name :: String        -- ^ Printable name of the event source.
   , typeInfo_source :: EventSource -- ^ Kind of the event source (hardware, software, tracepoint etc.).
   , typeInfo_id :: EventTypeID     -- ^ Unique number of this type of event.
   }

-- | Sort a list of events in ascending time order.
-- Events without a timestamp are treated as having a timestamp of 0,
-- which places them at the start of the sorted output.
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
   getEventTime :: EventPayload -> TimeStamp
   getEventTime e@(SampleEvent {}) = maybe (TimeStamp 0) id $ eventPayload_SampleTime e
   getEventTime e@(ForkEvent {}) = eventPayload_time e
   getEventTime e@(ExitEvent {}) = eventPayload_time e
   getEventTime e@(ThrottleEvent {}) = eventPayload_time e
   getEventTime e@(UnThrottleEvent {}) = eventPayload_time e
   getEventTime other = TimeStamp 0

-- | Build a map from "EventID"s to their type information.
makeTypeMap :: PerfData -> TypeMap
makeTypeMap perfData =
   List.foldl' idsToInfo Map.empty attrsIDs
   where
   -- Event IDs from the file header, to be associated with the event attributes
   -- from the header.
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
         -- but we prefer to ignore it rather than generate an error.
         Nothing -> acc
         -- Update the event type map, mapping each event id to the name.
         Just typeName ->
            Map.insert eventID (TypeInfo typeName typeSource typeID) acc
      where
      typeSource = ea_type attr
      typeID = ea_config attr

-- | Style to use for printing the event data.
data OutputStyle
   = Dump -- ^ Output full details of the data file preserving the original order of the events.
   | Trace -- ^ Output command and sample events in time order with event type annotations.

-- | Read the contents of the perf.data file and render it
-- on stdout in a specified style.
readAndDisplay :: OutputStyle -> FilePath -> IO ()
readAndDisplay style file = display style =<< readPerfData file

-- | Read and parse the perf.data file into its constituent components.
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
             []  -> (SampleTypeBitMap 0, False)
             x:_ -> x
       dataOffset = fh_data_offset header
       maxOffset = fh_data_size header + dataOffset
   events <- readEvents h maxOffset dataOffset sampleType
   return $ PerfData header attrs idss types events

-- | Render the components of the perf.data file under the specified style.
-- Don't create a single big @Doc@ or @String@ to avoid stack overflows.
-- Instead, lazily print events as they are rendered.
display :: OutputStyle -> PerfData -> IO ()
display style contents =
  let docs = case style of
        Dump -> dumper contents
        Trace -> tracer contents
  in mapM_ (putStrLn . render) docs

-- | Get the Sample Type and test the sample_id_all bit in the flags field.
getAttrInfo :: [FileAttr] -> [(SampleTypeBitMap, Bool)]
getAttrInfo = map getSampleTypeAndIdAll
   where
   getSampleTypeAndIdAll :: FileAttr -> (SampleTypeBitMap, Bool)
   getSampleTypeAndIdAll fattr
      = (ea_sample_type attr, testEventAttrFlag (ea_flags attr) SampleIdAll)
      where
      attr = fa_attr $ fattr

-- | Read the events from file and return them in the order that they appear
-- (not sorted on timestamp).
readEvents :: Handle -> ByteCount64 -> ByteCount64 -> SampleTypeBitMap -> IO [Event]
readEvents h maxOffset offset sampleType =
   readWorker offset []
   where
   readWorker :: ByteCount64 -> [Event] -> IO [Event]
   readWorker offset acc
      | offset >= maxOffset = return $ reverse acc
      | otherwise = do
           event <- readEvent h offset sampleType
           let size = eh_size $ ev_header event
               nextOffset = offset + fromIntegral size
           readWorker nextOffset (event:acc)

-- | Dump the events in a sequence, showing all their internal values.
dumper :: PerfData -> [Doc]
dumper (PerfData header attrs idss types events) =
   intersperse separator $
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

-- | Print a time sorted sequence of comm and sample events. Sample events
-- show their human-friendly source name.
tracer :: PerfData -> [Doc]
tracer perfData@(PerfData header attrs idss types events) =
   concatMap (prettyPayload typeMap . ev_payload) sortedEvents
   where
   -- Render a list of docs in comma separated form.
   csv :: [Doc] -> [Doc]
   csv docs = [hcat $ intersperse (text ", ") docs]
   sortedEvents = sortEventsOnTime events
   typeMap = makeTypeMap perfData
   prettyPayload :: TypeMap -> EventPayload -> [Doc]
   prettyPayload _typeMap ev@(CommEvent {}) =
      csv [ text "PID" <+> (pretty . eventPayload_pid) ev
          , text "TID" <+> (pretty . eventPayload_tid) ev
          , text "command" <+> (pretty . eventPayload_CommName) ev ]
   prettyPayload typeMap ev@(SampleEvent {}) =
      csv [ text "PID" <+> (pretty . eventPayload_SamplePID) ev
          , text "TID" <+> (pretty . eventPayload_SampleTID) ev
          , sampleDoc
          , text "time" <+> (pretty . eventPayload_SampleTime) ev
          , text "CPU" <+> (pretty . eventPayload_SampleCPU) ev
          , text "IP" <+> (pretty . eventPayload_SampleIP) ev
          , text "Addr" <+> (pretty . eventPayload_SampleAddr) ev
          , text "Stream" <+> (pretty . eventPayload_SampleStreamID) ev
          , text "Period" <+> (pretty . eventPayload_SamplePeriod) ev ]
      where
      -- Try to retrieve the sample event source from the type info for this event.
      sampleDoc
         | Just id <- eventPayload_SampleID ev,
           Just typeInfo <- Map.lookup id typeMap =
                text "sample" <+> (text . typeInfo_name) typeInfo
         | otherwise = text "sample <unknown source>"
   prettyPayload _typeMap other = []
