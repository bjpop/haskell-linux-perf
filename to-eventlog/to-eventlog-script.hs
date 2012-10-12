{-# LANGUAGE PatternGuards, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Convert linux perf data into a GHC eventlog, using the output from
-- perf script. You need to specify the name of the command that was
-- profiled as the first argument.
-- For example if the profiled command is called "Fac", and the perf data
-- is in a file called perf.data, we can generate a ghc log file like so:
--
--    to-eventlog-script Fac perf.data Fac.perf.eventlog
--
-----------------------------------------------------------------------------

import qualified GHC.RTS.Events as GHC
import Data.Map as Map hiding (mapMaybe, map, filter, null)
import Data.List as List (foldl')
import Data.Word (Word64, Word32)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, hGetContents)
import Data.Char (isDigit)
import System.Process

-- Select specific fields for perf script to display.
-- TODO: also specify different fields for software counters. This is
-- difficult due to perf script bugs.
perfScriptCmd :: String -> String
perfScriptCmd inFile =
   "perf script -f comm,tid,pid,time,cpu,event,trace -i " ++ inFile

main :: IO ()
main = do
   args <- getArgs
   case args of
      [command, inFile, outFile] -> do
         procOut <- createProcess (shell $ perfScriptCmd inFile)
                       { std_out = CreatePipe }
         case procOut of
            (_, Just hout, _, _) -> do
               -- read the stdout of perf script
               contents <- hGetContents hout
               -- Parse the perf events.
               -- TODO: should we report that we ignore some mis-formed lines?
               let perfEvents = mapMaybe parsePerfLine $ lines contents
               -- grab the start time of the first event for the command
               -- of interest
                   startTime = getStartTime command perfEvents
               -- convert the perf events into a GHC eventlog
                   perfEventlog = perfToEventlog startTime perfEvents
               -- debug: print the start time
               print startTime
               -- write the ghc eventlog to a file
               GHC.writeEventLogToFile outFile perfEventlog
            _ -> die "Internal error: shell process creation failed"
      _other -> die "Syntax: to-eventlog-script command [perf_file eventlog_file]"

-- exit the program with an error message
die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

getStartTime :: String -> [PerfEvent] -> Maybe Word64
getStartTime _command [] = Nothing
getStartTime  command (event:rest)
   | command == thisCommand = Just $ perfEvent_time event
   | otherwise = getStartTime command rest
   where
   thisCommand = perfEvent_command event

data PerfEvent =
   PerfEvent
   { perfEvent_command :: !String
   , perfEvent_threadID :: !Word64
   , _perfEvent_processID :: !Word64
   , perfEvent_time :: !Word64
   , _perfEvent_CPU :: !String
   , perfEvent_event :: !String
   , _perfEvent_trace :: !String
   }
   deriving (Eq, Show)

parsePerfLine :: String -> Maybe PerfEvent
parsePerfLine string
  | comm:ids:cpu:timeStrColon:eventColon:rest <- words string
  , (pidStr, _:tidStr) <- break (== '/') ids
  , (timeStr, ":")  <- break (== ':') timeStrColon
  , (topTime, _:botTime) <- break (== '.') timeStr
  , (event, ":")  <- break (== ':') eventColon = do
    timeMus <- safeReadInt (topTime ++ botTime)
    pid <- safeReadInt pidStr
    tid <- safeReadInt tidStr
    let trace = unwords rest
        -- Time resolution is 1000 lower than in Haskell eventlogs
        -- and in the raw, binary perf events format,
        -- hence we multiply by 1000.
        time = 1000 * timeMus
    return $ PerfEvent comm tid pid time cpu event trace
parsePerfLine _ = Nothing

safeReadInt :: String -> Maybe Word64
safeReadInt string
   | all isDigit string = Just $ read string
   | otherwise = Nothing

-- Convert linux perf event data into a ghc eventlog.
perfToEventlog :: Maybe Word64 -> [PerfEvent] -> GHC.EventLog
perfToEventlog mstart events =
   eventLog $ perfToGHC mstart events
   where
   eventLog :: [GHC.Event] -> GHC.EventLog
   eventLog evs = GHC.EventLog (GHC.Header perfEventlogHeader)
                               (GHC.Data evs)

type TypeMap = Map String Word32
type EventState = (TypeMap, [GHC.Event], Word32)

perfToGHC :: Maybe Word64   -- initial timestamp
          -> [PerfEvent]    -- perf events in sorted time order
          -> [GHC.Event]    -- ghc eventlog
perfToGHC mstart perfEvents =
   typeEvents ++ reverse ghcEvents
   where
   start = fromMaybe 0 mstart
   typeEvents :: [GHC.Event]
   typeEvents = mkTypeEvents $ Map.toList fullTypeMap
   -- we fold over the list of perf events and collect a set of
   -- event types and a list of ghc events
   (fullTypeMap, ghcEvents, _typeID) = List.foldl' perfToGHCWorker (Map.empty, [], 0) perfEvents
   -- convert the set of perf type infos into a list of events
   mkTypeEvents :: [(String, Word32)] -> [GHC.Event]
   mkTypeEvents = map (\(name, ident) -> GHC.Event 0 $ GHC.PerfName ident name)

   -- Extract a new type event and ghc event from the next perf event
   -- and update the state.
   -- Note: we need (some of) these bangs to avoid stack overflows.
   -- XXX a state monad would be nicer
   perfToGHCWorker :: EventState -> PerfEvent -> EventState
   perfToGHCWorker state@(!typeMap, !events, !typeID) !event
      -- only consider events after the program start time
      | eventTime >= start = (newTypeMap, newEvent:events, newTypeID)
      | otherwise = state
      where
      eventTime = perfEvent_time event
      relativeTime = eventTime - start
      eventName = perfEvent_event event
      (newTypeMap, ghcTypeID, newTypeID) =
         case Map.lookup eventName typeMap of
            -- We've not seen this event name before, allocate
            -- a new event ID for it and insert it into the type map.
            Nothing -> let nextTypeID  = typeID + 1
                           nextTypeMap = Map.insert eventName nextTypeID typeMap
                       in (nextTypeMap, nextTypeID, nextTypeID)
            -- We have seen this event before, return its typeID and
            -- do not update the typeMap or the type ID counter.
            Just thisTypeID -> (typeMap, thisTypeID, typeID)
      ghcTID = GHC.KernelThreadId $ perfEvent_threadID event
      -- generate the appropriate ghc event
      newEvent = GHC.Event relativeTime newEventBody
      newEventBody = GHC.PerfTracepoint ghcTypeID ghcTID

perfEventlogHeader :: [GHC.EventType]
perfEventlogHeader =
  [ GHC.EventType GHC.nEVENT_PERF_NAME "perf event name" Nothing
  , GHC.EventType GHC.nEVENT_PERF_COUNTER "perf event counter"
                  (Just $ GHC.sz_perf_num + GHC.sz_kernel_tid + 8)
  , GHC.EventType GHC.nEVENT_PERF_TRACEPOINT "perf event tracepoint"
                  (Just $ GHC.sz_perf_num + GHC.sz_kernel_tid)
  ]
