{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2010,2011,2012 Simon Marlow, Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Convert linux perf data into a GHC event log, using the output from
-- perf script. You need to specify the name of the command that was
-- profiled as the first argument:
--
-- For example if the profiled command is called "Fac", and the perf data
-- is in a file called perf.data, we can generate a ghc log file like so:
--
--    to-eventlog-script Fac perf.data ghc.data
--
-----------------------------------------------------------------------------

import qualified GHC.RTS.Events as GHC
import Data.Map as Map hiding (mapMaybe, map, filter, null)
import Data.List as List (foldl')
import Data.Word (Word64, Word32, Word16)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Char (isDigit)
import System.Process
import System.IO

-- Select specific fields for perf script to display
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
               -- parse the perf events
               let perfEvents = mapMaybe parsePerfLine $ lines contents
               -- grab the start time of the first event for the command
               -- of interest
                   startTime = getStartTime command perfEvents
               -- convert the perf events into a GHC log
                   perfEventlog = perfToEventlog startTime perfEvents
               -- print the start time
               print startTime
               -- write the ghc log to a file
               GHC.writeEventLogToFile outFile perfEventlog
      _other -> die "Syntax: to-eventlog-script command [perf_file eventlog_file]"

-- exit the program with an error message
die :: String -> IO a
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

getStartTime :: String -> [PerfEvent] -> Maybe Word64
getStartTime command [] = Nothing
getStartTime command (event:rest)
   | command == thisCommand = Just $ perfEvent_time event
   | otherwise = getStartTime command rest
   where
   thisCommand = perfEvent_command event

data PerfEvent =
   PerfEvent
   { perfEvent_command :: !String
   , perfEvent_threadID :: !Word64
   , perfEvent_processID :: !Word64
   , perfEvent_time :: !Word64
   , perfEvent_CPU :: !String
   , perfEvent_event :: !String
   , perfEvent_trace :: !String
   }
   deriving (Eq, Show)

-- XXX this is a bit hacky, more error checking is desirable
parsePerfLine :: String -> Maybe PerfEvent
parsePerfLine string
  | comm:ids:cpu:timeStr:eventStr:rest <- words string
  , (pidStr, _:tidStr) <- break (== '/') ids
  , (topTime, _:botTime) <- break (== '.') timeStr =
    let event = init eventStr
        trace = unwords rest
        -- Time resolution is 1000 lower than in Haskell eventlogs
        -- and in the raw, binary perf events format.
        time :: Word64
        time = 1000 * safeReadInt (topTime ++ init botTime)
        pid, tid :: Word64
        pid = safeReadInt pidStr
        tid = safeReadInt tidStr
    in Just $ PerfEvent comm tid pid time cpu event trace
parsePerfLine _ = Nothing

-- XXX icky hack to handle bad input
-- FIXME
safeReadInt :: String -> Word64
safeReadInt string
   | all isDigit string = read string
   | otherwise = -1

-- Convert linux perf event data into a ghc event log.
perfToEventlog :: Maybe Word64 -> [PerfEvent] -> GHC.EventLog
perfToEventlog mstart events =
   eventLog $ perfToGHC mstart events
   where
   eventLog :: [GHC.Event] -> GHC.EventLog
   eventLog events = GHC.EventLog (GHC.Header perfEventlogHeader)
                                  (GHC.Data events)

type TypeMap = Map String Word32
type EventState = (TypeMap, [GHC.Event], Word32)

perfToGHC :: Maybe Word64   -- initial timestamp
          -> [PerfEvent]    -- perf events in sorted time order
          -> [GHC.Event]    -- ghc event log
perfToGHC mstart perfEvents =
   typeEvents ++ reverse ghcEvents
   where
   start = fromMaybe 0 mstart
   typeEvents :: [GHC.Event]
   typeEvents = mkTypeEvents $ Map.toList typeMap
   -- we fold over the list of perf events and collect a set of
   -- event types and a list of ghc events
   (typeMap, ghcEvents, _typeID) = List.foldl' perfToGHCWorker (Map.empty, [], 0) perfEvents
   -- convert the set of perf type infos into a list of events
   mkTypeEvents :: [(String, Word32)] -> [GHC.Event]
   mkTypeEvents = map (\(name, id) -> GHC.Event 0 $ GHC.PerfName id name)

   -- extract a new type event and ghc event from the next perf event
   -- and update the state

   -- XXX a state monad would be nicer
   perfToGHCWorker :: EventState -> PerfEvent -> EventState
   perfToGHCWorker state@(typeMap, events, typeID) event
      -- only consider events after the start
      | eventTime >= start = (newTypeMap, newEvent:events, newTypeID)
      | otherwise = state
      where
      eventTime = perfEvent_time event
      relativeTime = eventTime - start
      eventName = perfEvent_event event
      (newTypeMap, ghcTypeID, newTypeID) =
         case Map.lookup eventName typeMap of
            -- XXX probably need a strict Map insert
            -- We've not seen this event name before, allocate
            -- a new event ID for it and insert it into the type map
            Nothing -> let nextTypeID  = typeID + 1
                           nextTypeMap = Map.insert eventName nextTypeID typeMap
                       in (nextTypeMap, nextTypeID, nextTypeID)
            -- We have seen this event before, return its typeID and
            -- do not update the typeMap or the type ID counter
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
