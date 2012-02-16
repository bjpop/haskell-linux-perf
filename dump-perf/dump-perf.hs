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
-- Usage: dump-perf <filename>
--
-- If filename is missing then it will assume the input is "perf.data" in
-- the current working directory.
--
-----------------------------------------------------------------------------

import Profiling.Linux.Perf
import System.Exit
import System.IO
import System.Environment
import Text.Printf
import Text.PrettyPrint
import Data.Word
import Data.List (intersperse)
import Data.Map as Map
import Data.ByteString.Lazy (ByteString)

data OutputStyle = Dump | Trace

die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  (outputStyle, file) <- case args of
     []     -> return (Dump, "perf.data")
     ["dump"] -> return (Dump, "perf.data")
     ["trace"] -> return (Trace, "perf.data")
     [file] -> return (Dump, file)
     ["dump", file]  -> return (Dump, file)
     ["trace", file] -> return (Trace, file)
     _               -> die "Syntax: dump-perf [dump|trace] [file]"
  display outputStyle file

display :: OutputStyle -> FilePath -> IO ()
display style file = do
   h <- openFile file ReadMode
   header <- readHeader h
   attrs <- readAttributes h header
   idss <- mapM (readAttributeIDs h) attrs
   types <- readEventTypes h header
   -- we assume the sampleType comes from the first attr
   -- it is not clear what to do if there is more than one, or even if that is valid.
   -- See: samplingType in perffile/session.c and the way it is set in the CERN readperf code.
   -- They also assume there is just one sampleType.
   let sampleType =
          case attrs of
             [] -> 0 -- assume none of the sample types are set
             firstAttr:_ -> ea_sample_type $ fa_attr $ firstAttr
       dataOffset = fh_data_offset header
       maxOffset = fh_data_size header + dataOffset
   events <- readEvents h maxOffset dataOffset sampleType
   putStrLn $ render $ case style of
      Dump ->  dumper header attrs idss types events
      Trace -> tracer header attrs idss types events

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

separator :: Doc
separator = text $ Prelude.replicate 40 '-'

dumper :: FileHeader -> [FileAttr] -> [[Word64]] -> [TraceEventType] -> [Event] -> Doc
dumper header attrs idss types events =
   vcat $ intersperse separator $
      [ text "Perf File Header:"
      , pretty header
      , text "Perf File Attributes:"
      , vcat $ intersperse separator $
               Prelude.map prettyAttrAndIds $ Prelude.zip attrs idss
      , text "Trace Event Types:"
      , vcat $ Prelude.map pretty types
      , text "Events:"
      ] ++ Prelude.map pretty events
   where
   prettyAttrAndIds (attr, ids) =
      pretty attr $$ (text "ids:" <+> (hsep $ Prelude.map pretty ids))

tracer :: FileHeader -> [FileAttr] -> [[Word64]] -> [TraceEventType] -> [Event] -> Doc
tracer header attrs idss types events =
   vcat $ traceSamples Map.empty attrsMap $ Prelude.map ev_payload events
   where
   -- mapping from type id to type name
   typesMap :: Map Word64 ByteString
   typesMap = fromList typesIDsNames
   typesIDsNames = Prelude.map (\t -> (te_event_id t, te_name t)) types
   -- mapping from event id to type name
   attrsMap :: Map Word64 ByteString
   attrsMap = makeAttrsMap $ zip (Prelude.map fa_attr attrs) idss
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
   doc : traceSamples (insert (pid,tid) command idMap) attrMap rest
   where
   doc = text "command:" <+> pretty command <+>
         text "pid:" <+> prettyIntegral pid <+>
         text "tid:" <+> prettyIntegral tid
   command = ce_comm ce
   tid = ce_tid ce
   pid = ce_pid ce
traceSamples idMap attrMap (se@SampleEvent {} : rest) =
   doc : traceSamples idMap attrMap rest
   where
   doc = processName <+> sampleType <+> timeStamp
   processName =
      case (se_pid se, se_tid se) of
         (Nothing, Nothing) -> text "unknown (pid, tid)"
         (Nothing, _) -> text "unknown pid"
         (_, Nothing) -> text "unknown tid"
         (Just pid, Just tid) ->
            case Map.lookup (pid, tid) idMap of
               -- Nothing -> text "(" <> prettyIntegral pid <> text "," <> prettyIntegral tid <> text ")"
               Nothing -> pretty (pid, tid) 
               Just name -> pretty name
   sampleType =
      case se_id se of
         Nothing -> text "unknown sample id"
         Just sid ->
            case Map.lookup sid attrMap of
               Nothing -> prettyIntegral sid
               Just name -> pretty name
   timeStamp =
      case se_time se of
         Nothing -> text "uknown time"
         Just time -> prettyIntegral time
traceSamples idMap attrMap (_otherSample : rest) =
  traceSamples idMap attrMap rest
