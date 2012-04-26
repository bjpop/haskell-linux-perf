-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-----------------------------------------------------------------------------

import GHC.RTS.Events
import Data.Word

import Profiling.Linux.Perf (PerfFileContents, readPerfData)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)

die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  files <- case args of
    []             -> return $ Just ("perf.data", "test.eventlog")
    ["--no-files"] -> return $ Nothing
    [inF, outF]    -> return $ Just (inF, outF)
    _ -> die "Syntax: to-eventlog [--no-files|perf_file eventlog_file]"
  case files of
    Nothing -> putStr $ ppEventLog test
    Just (inF, outF) -> do
      perfData <- readPerfData inF
      let perfEventlog = perfToEventlog perfData
      writeEventLogToFile outF perfEventlog

-- type PerfFileContents = (FileHeader, [FileAttr], [[Word64]], [TraceEventType], [Event])
perfToEventlog :: PerfFileContents -> EventLog
perfToEventlog (_, _, _, _, _) = test  -- TODO

test :: EventLog
test = eventLog $
  [ Event 0 (PerfName 0 "L2 cache misses")
  , Event 1000 (PerfCounter 0 1)
  , Event 1100 (PerfCounter 0 2)
  , Event 2000 (PerfName 1 "kill")
  , Event 2100 (PerfCounter 0 3)
  , Event 2200 (PerfTracepoint 1 0)
  ]

eventLog :: [Event] -> EventLog
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
