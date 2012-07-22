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
-- rec-perf --RTS [ +RecPerf [rec-perf-args ... ] -RecPerf ] command [command-args ... ]
--
-- The --RTS is to stop ghc from grabbing any +RTS ... -RTS commands from
-- the command line.
--
-- perf will write its output to the file "perf.data".
--
-- TODO:
--    - command line arguments to allow user to specify a set of events to
--      to trace instead of the default ones.
--    - decide what to do if we are waiting for a process and it stops
--      instead of terminates/exits.
--    - decide what to do if perf terminates before the child
--
-----------------------------------------------------------------------------

module Main where

import System.Posix.Types
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

main :: IO ()
main = do
   argv <- getArgs
   let (recPerfArgv, otherArgv) = grabRecPerfArgv argv
   recPerfOptions <- parseRecPerfOptions recPerfArgv
   profileCommand recPerfOptions otherArgv

profileCommand :: Options -> [String] -> IO ()
profileCommand _options [] = ioError $ userError ("You did not supply a command to profile")
profileCommand options (childCommand:childArgs) = do
   childPath <- checkChildCommand childCommand
   -- start a process for the command to be profiled
   childPID <- forkProcess $ childProcess childPath childArgs
   -- start a perf process attached to the childCommand 
   perfPID <- forkProcess $ perfProcess options childPID
   -- wait for the child to terminate
   waitForProcessTerminate childPID
   -- terminate the perf process
   -- perf seems to respect keyboard signal and flush its buffers
   signalProcess keyboardSignal perfPID
   -- wait for perf to terminate
   -- XXX what if this process ends before the child? 
   waitForProcessTerminate perfPID
   return ()

-- Check if the child command exists and is executable.
checkChildCommand :: FilePath -> IO FilePath
checkChildCommand childCommand = do
   let (childCommandDir, childCommandFile) = splitFileName childCommand
   childPath <-
      if null childCommandDir
         -- child command was not prefixed with a directory path
         then do
            -- try to look up the command in the PATH environment
            maybeChildPath <- findExecutable childCommand
            case maybeChildPath of
               Nothing -> ioError $ userError ("Command: " ++ childCommand ++ " not found")
               Just childPath -> return childPath
      else
         return childCommand
   exists <- doesFileExist childPath 
   if exists
      then do
         -- check if we can execute the command
         perms <- getPermissions childPath	
         if executable perms
            then return childPath
            else ioError $ userError ("You do not have permission to execute command: " ++ childPath)
      else ioError $ userError ("File: " ++ childPath ++ " does not exist")

data Options = Options
   { options_wait :: Integer
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

defaultWait :: Integer
defaultWait = 500000

defaultPerfOutputFile :: FilePath
defaultPerfOutputFile = "perf.data"

parseRecPerfOptions :: [String] -> IO Options
parseRecPerfOptions argv =
   case getOpt Permute options argv of
      (foundOptions, _unknowns, errors@[]) -> do
         let options = foldl (flip id) defaultOptions foundOptions
         if options_help options
            then do
               putStrLn usage
               exitSuccess
            else return options
      (_flags, _unknowns , errs@(_:_)) -> 
         ioError $ userError (concat errs ++ usage)

usage :: String
usage = usageInfo header options

header :: String
header = "Usage: rec-perf --RTS [ +RecPerf [rec-perf-args ... ] -RecPerf ] command [command-args ... ]"

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

safeReadInt :: String -> String -> Integer
safeReadInt option cs
   | length cs > 0 && all isDigit cs = read cs
   | otherwise =
        error ("The argument for option " ++ option ++
               " should be an integer, but it was " ++ cs)

-- Cut out all the arguments between +RecPerf -RecPerf from the command
-- line. Return two lists: 1) everything between the markers, and 2) everything else
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

childProcess :: FilePath -> [String] -> IO ()
childProcess command args = do
   -- wait for a short time (0.5 seconds) to allow perf to start recording this process
   threadDelay 500000
   -- run the command to be profiled
   executeFile command True args Nothing

perfProcess :: Options -> ProcessID -> IO ()
perfProcess options pid = do
    executeFile "perf" True
       (["record",

        -- record on all CPUs

        "-a", 

        -- output to this file

        "-o", options_output options,

        -- count every event

        "-c", "1", 

        -- attach to the PID of the profilee

        "-p", show pid, 

        -- scheduler events to record

        "-e", "sched:sched_process_exit",
        "-e", "sched:sched_kthread_stop",
        "-e", "sched:sched_kthread_stop_ret",
        "-e", "sched:sched_wakeup",
        "-e", "sched:sched_wakeup_new",
        "-e", "sched:sched_switch",
        "-e", "sched:sched_migrate_task",
        "-e", "sched:sched_process_free",
        "-e", "sched:sched_process_exit",
        "-e", "sched:sched_wait_task",
        "-e", "sched:sched_process_wait",
        "-e", "sched:sched_process_fork",
        "-e", "sched:sched_stat_wait",
        "-e", "sched:sched_stat_sleep",
        "-e", "sched:sched_stat_iowait",
        "-e", "sched:sched_stat_runtime",
        "-e", "sched:sched_pi_setprio",

        -- system calls to record

        "-e", "raw_syscalls:sys_enter",
        "-e", "raw_syscalls:sys_exit", 
        "-e", "syscalls:sys_enter_gettimeofday",
        "-e", "syscalls:sys_exit_gettimeofday"

        -- dummy process.
        -- Perf terminates if this process terminates.
        -- sleep gives us an upper bound on the
        -- tracing operation, which is probably a
        -- useful safeguard. Very long traces could
        -- generate large files, which are dangerous
        -- if run as root (which is commonly required). 

       ] ++ (words $ options_dummy options))

       Nothing
