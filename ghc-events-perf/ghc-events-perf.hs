-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2012 Duncan Coutts, Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A tool to help profiling a Haskell program using Linux 'perf',
-- ghc-events and, eventually, Threadscope.
--
-- The ghc-events-perf tool has a couple of commands
-- for obtaining 'perf' performance data for a Haskell program,
-- translating it to the ghc-events format, sycnronising and merging
-- with the standard ghc-events eventlog for the Haskell program,
-- and so making the data ready for display in Threadscope.
--
-- Usage:
-- ghc-events-perf command command-args
--
-- To get help:
-- ghc-events-perf help
-- ghc-events-perf help command
-----------------------------------------------------------------------------

module Main where

import System.Posix.Process
import System.Environment
import System.Exit
import System.IO (hPutStrLn, stderr)
import Control.Monad

main :: IO ()
main = getArgs >>= command

command :: [String] -> IO ()
command [helpString] | helpString `elem` ["-h", "--help", "help"] =
  putStrLn usage

command ["help", "help"] =
  putStrLn "Helps."

command ["help", "record"] = do
  let args = ["+GhcEventsPerf", "-h"]
  executeFile "ghc-events-perf-record" True args Nothing

command ["help", "convert"] =
  putStr $ unlines $
    [ "Usage: ghc-events-perf convert program-name [out-file perf-file eventlog-file]"
    , "  program-name   the name of the profiled Haskell program"
    , "  out-file       where to save the resulting compound eventlog"
    , "  perf-file      path to the perf data file for the Haskell program"
    , "  eventlog-file  path to the GHC RTS eventlog of the Haskell program"
    ]

-- TODO: RTS options intended for ghc-events-perf-record are consumed
-- in ghc-events-perf instead. (But profilee options are safely escaped.)
command ("record" : args) =
  executeFile "ghc-events-perf-record" True args Nothing

command ["convert", program_name, out_file, perf_file, eventlog_file] = do
  let sync_eventlog_file = program_name ++ ".perf.eventlog"
      syncArgs = [ program_name
                 , perf_file
                 , sync_eventlog_file
                 ]
  void $ executeFile "ghc-events-perf-sync" True syncArgs Nothing
  let mergeArgs = [ "merge"
                  , out_file
                  , sync_eventlog_file
                  , eventlog_file
                  ]
  executeFile "ghc-events" True mergeArgs Nothing

command ["convert", program_name] = do
  let out_file = program_name ++ ".total.eventlog"
      perf_file = "data.perf"  -- the same as defaultPerfOutputFile in record
      eventlog_file = program_name ++ ".eventlog"
  command ["convert", program_name, out_file, perf_file, eventlog_file]

command _ = putStrLn usage >> die "Unrecognized command"

usage :: String
usage = "Usage: ghc-events-perf command command-args\n\nThe available commands are: record convert help.\n\nSee 'ghc-events-perf help command' for more information on a specific command."

-- Exit the program with an error message.
die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)
