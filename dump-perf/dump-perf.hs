import Profiling.Linux.Perf
import System.Exit
import System.IO
import System.Environment
import Text.Printf
import Text.PrettyPrint
import Data.Word

main = do
  args <- getArgs
  file <- case args of
            []     -> return "perf.data"
            [file] -> return file
            _      -> die "Syntax: dump-perf [file]"
  dumper file

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)

separator :: IO ()
separator = printf "%s\n" $ Prelude.replicate 40 '-'

-- dumper :: FilePath -> IO (FileHeader, [FileAttr])
dumper :: FilePath -> IO ()
dumper f = do
   h <- openFile f ReadMode
   header <- readHeader h
   attrs <- readAttributes h header
   idss <- mapM (readAttributeIDs h) attrs
   separator
   printf "Perf File Header:\n"
   separator
   printf "%s\n" $ prettyString header
   separator
   printf "Perf File Attributes:\n"
   separator
   let prettyAttrAndIds (attr, ids) = pretty attr $$ (text "ids:" <+> (hsep $ Prelude.map pretty ids))
   printf "%s\n" $ render $ vcat $ Prelude.map prettyAttrAndIds $ Prelude.zip attrs idss
   -- eventHeader <- readEventHeader h $ fh_data_offset header
   let dataOffset = fh_data_offset header
       maxOffset = fh_data_size header + dataOffset
       sampleType = ea_sample_type (fa_attr (attrs !! 0)) -- XXX must check attrs is not empty
   dumpEvents h maxOffset dataOffset sampleType
{-
   event <- readEvent h $ fh_data_offset header
   separator
   printf "Perf Event:\n"
   separator
   printf "%s\n" $ prettyString event
-}

dumpEvents :: Handle -> Word64 -> Word64 -> Word64 -> IO ()
dumpEvents h maxOffset offset sampleType
   | offset >= maxOffset = return ()
   | otherwise = do
        event <- readEvent h offset sampleType
        separator
        printf "Perf Event:\n"
        separator
        printf "%s\n" $ prettyString event
        let size = eh_size $ ev_header event 
            nextOffset = offset + fromIntegral size
        dumpEvents h maxOffset nextOffset sampleType
