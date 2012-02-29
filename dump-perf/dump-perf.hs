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
-- Usage: dump-perf [--dump|--trace] [file]
--
-- If filename is missing then it will assume the input is "perf.data" in
-- the current working directory.
--
-----------------------------------------------------------------------------

import Profiling.Linux.Perf (OutputStyle (..), readAndDisplay)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)

die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  (outputStyle, file) <- case args of
     []        -> return (Dump, "perf.data")
     ["--dump"]  -> return (Dump, "perf.data")
     ["--trace"] -> return (Trace, "perf.data")
     [file] -> return (Dump, file)
     ["--dump", file]  -> return (Dump, file)
     ["--trace", file] -> return (Trace, file)
     _               -> die "Syntax: dump-perf [--dump|--trace] [file]"
  readAndDisplay outputStyle file
