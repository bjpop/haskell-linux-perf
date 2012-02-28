-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope 
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A program to parse and then pretty print the contents of "perf.data" to
-- stdout. "perf.data" is the the output of the "perf record" command on
-- linux (linux performance counter information).
--
-- The main use of this program is to demonstrate how to use the
-- Profilinf.Linux.Perf library.
--
-- Usage: dump-perf <dump|trace> <filename>
--
-- If filename is missing then it will assume the input is "perf.data" in
-- the current working directory.
--
-----------------------------------------------------------------------------

module Profiling.Linux.Perf
   ( module Profiling.Linux.Perf.Parse
   , PerfFileContents
   , OutputStyle (..)
   , readAndDisplay
   , readPerfData
   , display
   ) where

import Profiling.Linux.Perf.Parse

-- import Text.Printf
import Text.PrettyPrint as Pretty
import Data.Word
import Data.List (intersperse, sortBy)
import Data.Map as Map hiding (map, filter, null)
import Data.ByteString.Lazy (ByteString)
import Data.Bits (testBit)
import System.IO

data OutputStyle = Dump | Trace

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
   let (sampleType, sampleIdAll) = getSampleTypeAndIdAll attrs
       dataOffset = fh_data_offset header
       maxOffset = fh_data_size header + dataOffset
   events <- readEvents h maxOffset dataOffset sampleType
   return (header, attrs, idss, types, events)

-- Render the components of the perf.data file under the specified style
display :: OutputStyle -> PerfFileContents -> IO () 
display style contents = do
   putStrLn $ render $ case style of
      Dump ->  dumper contents
      Trace -> tracer contents

-- we assume the sampleType comes from the first attr
-- it is not clear what to do if there is more than one, or even if that is valid.
-- See: samplingType in perffile/session.c and the way it is set in the CERN readperf code.
-- They also assume there is just one sampleType.
getSampleTypeAndIdAll :: [FileAttr] -> (Word64, Bool)
getSampleTypeAndIdAll attrs
   | null attrs = (0, False) -- assume none of the sample types are set
   | otherwise = (ea_sample_type attr, testBit (ea_flags attr) sampleIdAllPos)
   where
   attr = fa_attr $ head attrs

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

-- Pretty print the events in sorted timestamp order, mapping events to their
-- types and PIDs to their command names.
tracer :: PerfFileContents -> Doc
tracer (header, attrs, idss, types, events) =
   vcat $ traceSamples Map.empty attrsMap
        $ sortBy compareSamplePayload
        $ map ev_payload events
   where
   -- mapping from type id to type name
   typesMap :: Map Word64 ByteString
   typesMap = fromList typesIDsNames
   typesIDsNames = map (\t -> (te_event_id t, te_name t)) types
   -- mapping from event id to type name
   attrsMap :: Map Word64 ByteString
   attrsMap = makeAttrsMap $ zip (map fa_attr attrs) idss
   makeAttrsMap :: [(EventAttr, [Word64])] -> Map Word64 ByteString
   makeAttrsMap = foldr idsToName Map.empty
   idsToName :: (EventAttr, [Word64]) -> Map Word64 ByteString -> Map Word64 ByteString
   idsToName (attr, ids) result =
      case Map.lookup (ea_config attr) typesMap of
         Nothing -> result
         Just name -> foldr (flip Map.insert name) result ids

prettyIntegral :: Integral a => a -> Doc
prettyIntegral = int . fromIntegral

traceSamples :: Map (Word32, Word32) ByteString -> Map Word64 ByteString -> [EventPayload] -> [Doc]
traceSamples _idMap _attrMap [] = []
traceSamples idMap attrMap (ee@ExitEvent {} : rest) =
   doc : traceSamples idMap attrMap rest
   where
   doc = text "exit:" <+> parent <+> text "->" <+> child <+> timeStamp
   parent = pretty (ee_ppid ee, ee_ptid ee) 
   child = pretty (ee_pid ee, ee_tid ee)
   timeStamp = prettyIntegral $ ee_time ee
traceSamples idMap attrMap (fe@ForkEvent {} : rest) =
   doc : traceSamples idMap attrMap rest
   where
   doc = text "fork:" <+> parent <+> text "->" <+> child <+> timeStamp
   parent = pretty (fe_ppid fe, fe_ptid fe) 
   child = pretty (fe_pid fe, fe_tid fe)
   timeStamp = prettyIntegral $ fe_time fe
traceSamples idMap attrMap (ce@CommEvent {} : rest) =
   -- doc : traceSamples (insert (pid,tid) command idMap) attrMap rest
   traceSamples (insert (pid,tid) command idMap) attrMap rest
   where
{-
   doc = text "command:" <+> pretty command <+>
         text "pid:" <+> prettyIntegral pid <+>
         text "tid:" <+> prettyIntegral tid
-}
   command = ce_comm ce
   tid = ce_tid ce
   pid = ce_pid ce
traceSamples idMap attrMap (se@SampleEvent {} : rest) =
   doc : traceSamples idMap attrMap rest
   where
   doc = processName <+> pid <+> cpu <+> sampleType <+> timeStamp
   processName =
      case (se_pid se, se_tid se) of
         (Nothing, Nothing) -> text "unknown (pid, tid)"
         (Nothing, _) -> text "unknown pid"
         (_, Nothing) -> text "unknown tid"
         (Just pid, Just tid) ->
            case Map.lookup (pid, tid) idMap of
               Nothing -> pretty (pid, tid) 
               Just name -> pretty name
   sampleType =
      case se_id se of
         Nothing -> text "unknown sample id"
         Just sid ->
            case Map.lookup sid attrMap of
               Nothing -> prettyIntegral sid
               Just name -> pretty name
   timeStamp = maybe Pretty.empty prettyIntegral $ se_time se
   cpu = text "cpu=" <> (maybe Pretty.empty prettyIntegral $ se_cpu se)
   pid = text "pid=" <> (maybe Pretty.empty prettyIntegral $ se_pid se)
traceSamples idMap attrMap (_otherSample : rest) =
  traceSamples idMap attrMap rest

isSampleEvent :: EventPayload -> Bool
isSampleEvent (SampleEvent {}) = True
isSampleEvent _other = False

-- Get the timestamp of an event if it has one, otherwise
-- set it to 0 (for the purposes of sorting them).
getEventTime :: EventPayload -> Word64
getEventTime e@(SampleEvent {}) = maybe 0 id $ se_time e
getEventTime e@(ForkEvent {}) = fe_time e
getEventTime e@(ExitEvent {}) = ee_time e
getEventTime e@(ThrottleEvent {}) = te_time e
getEventTime e@(UnThrottleEvent {}) = ue_time e
getEventTime other = 0

-- Compare two events based on their timestamp.
compareSamplePayload :: EventPayload -> EventPayload -> Ordering
compareSamplePayload e1 e2 = compare (getEventTime e1) (getEventTime e2)
