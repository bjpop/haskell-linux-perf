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

main :: IO ()
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

-- Read the contents of the perf.data file and pretty print it to
-- standard output.
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
   -- we assume the sampleType comes from the first attr
   -- it is not clear what to do if there is more than one, or even if that is valid.
   let sampleType =
          case attrs of
             [] -> 0 -- assume none of the sample types are set
             firstAttr:_ -> ea_sample_type $ fa_attr $ firstAttr
       dataOffset = fh_data_offset header
       maxOffset = fh_data_size header + dataOffset
   dumpEvents h maxOffset dataOffset sampleType

-- Read the stream of data samples and pretty print them to stdout.
dumpEvents :: Handle -> Word64 -> Word64 -> Word64 -> IO ()
dumpEvents h maxOffset offset sampleType
   | offset >= maxOffset = return ()
   | otherwise = do
        event <- readEvent h offset sampleType
        separator
        printf "Perf Event:\n"
        separator
        printf "%s\n" $ prettyString event
        -- Calculate the file offset for the next sample.
        let size = eh_size $ ev_header event 
            nextOffset = offset + fromIntegral size
        dumpEvents h maxOffset nextOffset sampleType
