-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2012 Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A program to "perf record" a trace of another command.
--
-- The linux performance counter tool "perf" can attach to a running process
-- and record events for just that PID. This program automates that activity
-- for a given command, which we will call the profilee. First it forks a
-- process for the profilee, then it forks another process which attaches
-- "perf record" onto the PID of the profilee. It then waits for the
-- profilee to terminate, and then terminates the perf process. Note that
-- the profilee process sleeps for a short time before running the desired
-- command. This gives the perf process time to start up and capture all the
-- events in the profilee (otherwise we would have a condition where
-- the profilee could start or even terminate before perf had begun
-- recording events). Also note that when "perf record" is used in PID mode
-- it requires another dummy command which it uses to decide when to stop
-- recording - that is, when the dummy process ends so does the perf process.
-- We use the unix "sleep" command as the dummy process. Currently it is set
-- to run for 60 seconds, but we should make it a parameter to rec-perf.
--
-- Usage:
-- rec-perf [--RTS]
--          [ +RecPerf [rec-perf-args ... ] -RecPerf ]
--          command [command-args ... ]
--
-- To get help:
-- rec-perf +RecPerf -h
--
-- The --RTS is to stop ghc from grabbing any +RTS ... -RTS commands from
-- the command line.
--
-- TODO:
--    - decide what to do if we are waiting for a process and it stops
--      instead of terminates/exits.
--    - decide what to do if perf terminates before the profilee
--
-----------------------------------------------------------------------------

module Main where

import System.Posix.Types
import System.Posix.Unistd
import System.Posix.Process
import System.Posix.Signals
import System.Environment
import Control.Concurrent
import Control.Monad (when)
import System.Exit
import System.Console.GetOpt
import Data.Char (isDigit)
import System.Directory
   (findExecutable, getPermissions, executable, doesFileExist)
import System.FilePath (splitFileName)
import GetTimeOfDay (getTimeOfDay)

main :: IO ()
main = do
   argv <- getArgs
   let (recPerfArgv, profileeArgv) = grabRecPerfArgv argv
   recPerfOptions <- parseRecPerfOptions recPerfArgv
   profileCommand recPerfOptions profileeArgv

profileCommand :: Options -> [String] -> IO ()
profileCommand _options [] = ioError $ userError ("You did not supply a command to profile")
profileCommand options (profileeCommand:profileeArgs) = do
   profileePath <- checkProfileeCommand profileeCommand
   -- start a process for the command to be profiled
   profileePID <- forkProcess $ profileeProcess options profileePath profileeArgs
   -- start a perf process attached to the profileeCommand
   perfPID <- forkProcess $ perfProcess options profileePID
   -- wait for the profilee to terminate
   waitForProcessTerminate profileePID
   -- terminate the perf process
   -- perf seems to respect keyboard signal and flush its buffers
   signalProcess keyboardSignal perfPID
   -- wait for perf to terminate
   -- XXX what if this process ends before the profilee?
   waitForProcessTerminate perfPID
   return ()

-- Check if the profilee command exists and is executable.
checkProfileeCommand :: FilePath -> IO FilePath
checkProfileeCommand profileeCommand = do
   let (profileeCommandDir, profileeCommandFile) = splitFileName profileeCommand
   profileePath <-
      if null profileeCommandDir
         -- profilee command was not prefixed with a directory path
         then do
            -- try to look up the command in the PATH environment
            maybeProfileePath <- findExecutable profileeCommand
            case maybeProfileePath of
               Nothing -> ioError $ userError ("Command: " ++ profileeCommand ++ " not found")
               Just profileePath -> return profileePath
      else
         return profileeCommand
   exists <- doesFileExist profileePath
   if exists
      then do
         -- check if we can execute the command
         perms <- getPermissions profileePath
         if executable perms
            then return profileePath
            else ioError $ userError ("You do not have permission to execute command: " ++ profileePath)
      else ioError $ userError ("File: " ++ profileePath ++ " does not exist")

-- Options for rec-perf itself, some of which are passed on to "perf record"
data Options = Options
   { options_wait :: Int
   , options_events :: [String]
   , options_dummy :: String
   , options_output :: FilePath
   , options_help :: Bool
   } deriving Show

defaultOptions :: Options
defaultOptions = Options
   { options_wait = defaultWait
   , options_events = []
   , options_dummy = defaultDummyCommand
   , options_output = defaultPerfOutputFile
   , options_help = False
   }

defaultDummyCommand :: String
defaultDummyCommand = "sleep 60"

-- half a second
defaultWait :: Int
defaultWait = 500000

defaultPerfOutputFile :: FilePath
defaultPerfOutputFile = "perf.data"

-- parse the command line arguments that appeared between +RefPerf and -RecPerf
parseRecPerfOptions :: [String] -> IO Options
parseRecPerfOptions argv =
   case getOpt Permute options argv of
      (foundOptions, _unknowns, errors@[]) -> do
         let options = foldl (flip id) defaultOptions foundOptions
         if options_help options
            then putStrLn usage >> exitSuccess
            else return options
      (_flags, _unknowns , errs@(_:_)) ->
         ioError $ userError (concat errs ++ usage)

usage :: String
usage = usageInfo header options

header :: String
header = "Usage: rec-perf [--RTS] [ +RecPerf [rec-perf-args ... ] -RecPerf ] command [command-args ... ]"

options :: [OptDescr (Options -> Options)]
options =
      [ Option
           "w" ["wait"]
           (ReqArg (\i opts -> opts { options_wait = safeReadInt "wait" i }) "T")
           ("Time in microseconds to pause profilee to allow perf to attach to it. " ++
           "Defaults to " ++ show defaultWait ++ ".")

      , Option
           "e" ["event"]
           (ReqArg (\e opts -> opts { options_events = e : options_events opts }) "E")
           "Event to trace, instead of default events. Can be specified multiple times."

      , Option
           "d" ["dummy"]
           (ReqArg (\s opts -> opts { options_dummy = s }) "C")
           ("Dummy command to control how long perf-record runs for. " ++
           "Defaults to " ++ show defaultDummyCommand ++ ".")

      , Option
           "h" ["help"]
           (NoArg $ \opts -> opts { options_help = True })
           ("Display a help message for this program.")

      , Option
           "o" ["output"]
           (ReqArg (\f opts -> opts { options_output = f }) "OUT")
           ("Output file to store perf record data. " ++
           "Defaults to " ++ show defaultPerfOutputFile ++ ".")
      ]

-- Check that a string can be safely intepreted as an integer,
-- otherwise fail with an error message.
safeReadInt :: String -> String -> Int
safeReadInt option cs
   | length cs > 0 && all isDigit cs = read cs
   | otherwise =
        error ("The argument for option " ++ option ++
               " should be an integer, but it was " ++ cs)

-- Cut out all the arguments between +RecPerf -RecPerf from the command
-- line. Return two lists: 1) everything between the markers, and 2) everything else.
grabRecPerfArgv :: [String] -> ([String], [String])
grabRecPerfArgv cmdline =
   (reverse ins, reverse outs)
   where
   (ins, outs) = outside cmdline ([], [])
   outside, inside :: [String] -> ([String], [String]) -> ([String], [String])
   outside [] acc = acc
   outside ("+RecPerf":rest) acc = inside rest acc
   outside (str:rest) (ins, outs) = outside rest (ins, str:outs)
   inside [] acc = acc
   inside ("-RecPerf":rest) acc = outside rest acc
   inside (str:rest) (ins, outs) = inside rest (str:ins, outs)

-- Block the parent process by waiting for a child to terminate.
waitForProcessTerminate :: ProcessID -> IO ()
waitForProcessTerminate pid = do
   processStatus <- getProcessStatus
                       True -- block this process and wait
                       True -- XXX what is this for?
                       pid
   case processStatus of
      -- no status available for the process, try to kill it instead. XXX is this necessary?
      Nothing -> signalProcess killProcess pid
      Just (Exited code) -> return ()
      Just (Terminated signal) -> return ()
      Just (Stopped signal) ->
         -- XXX what to do here?
         return ()

-- Run the profilee process after a short wait to allow perf to attach to the process ID.
profileeProcess :: Options -> FilePath -> [String] -> IO ()
profileeProcess options command args = do
   -- wait for a short time to allow perf to start recording this process
   threadDelay $ options_wait options
   -- call gettimeofday to synchronise times
   t <- getTimeOfDay
   print t
   nanosleep 3
   -- run the command to be profiled
   executeFile command True args Nothing

-- Attach "perf record" to the profilee process ID.
perfProcess :: Options -> ProcessID -> IO ()
perfProcess options pid = do
   executeFile "perf" True (command ++ args) Nothing
   where
   command = ["record"]
   args = concat [allCores, output, frequency, attachPID, selectedEvents, dummyCommand]
   allCores = ["-a"]
   output = ["-o", options_output options]
   frequency = ["-c", "1"]
   attachPID = ["-p", show pid]
   dummyCommand = words $ options_dummy options
   -- If no events were specified on the command line then use the defaults
   selectedEvents
      | null optionEvents = mkEventFlags defaultEvents
      | otherwise = mkEventFlags optionEvents
      where
      optionEvents = options_events options
   mkEventFlags :: [String] -> [String]
   mkEventFlags = alternate (repeat "-e")

-- Record these events by default unless the user specifies alternatives.
defaultEvents :: [String]
defaultEvents =
   [
    -- scheduler events to record
    "sched:sched_process_exit",
    "sched:sched_kthread_stop",
    "sched:sched_kthread_stop_ret",
    "sched:sched_wakeup",
    "sched:sched_wakeup_new",
    "sched:sched_switch",
    "sched:sched_migrate_task",
    "sched:sched_process_free",
    "sched:sched_process_exit",
    "sched:sched_wait_task",
    "sched:sched_process_wait",
    "sched:sched_process_fork",
    "sched:sched_stat_wait",
    "sched:sched_stat_sleep",
    "sched:sched_stat_iowait",
    "sched:sched_stat_runtime",
    "sched:sched_pi_setprio",

    -- system calls to record
    "raw_syscalls:sys_enter",
    "raw_syscalls:sys_exit",
    "syscalls:sys_enter_gettimeofday",
    "syscalls:sys_exit_gettimeofday",
    "syscalls:sys_enter_nanosleep",
    "syscalls:sys_exit_nanosleep"
   ]

-- Given two lists [a, b, c ..] [d, e, f ..]
-- return a single list by alternating elements from
-- each: [a, d, b, e, c, f ..] until at least one
-- of the lists is exhausted.
alternate :: [a] -> [a] -> [a]
alternate [] _ = []
alternate _ [] = []
alternate (x:xs) (y:ys) = x : y : alternate xs ys
