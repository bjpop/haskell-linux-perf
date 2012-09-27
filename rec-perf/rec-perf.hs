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
-- The linux performance counter tool "perf" can record events
-- for a given command. This program runs "perf" adding our default set
-- of options. In particular, it specifies our default set
-- of events to be recorded.
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
-----------------------------------------------------------------------------

module Main where

import System.Posix.Process
import System.Environment
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
   -- debug
   t <- getTimeOfDay
   print t
   -- run perf record with the profilee command
   perfProcess options profileePath profileeArgs

-- Check if the profilee command exists and is executable.
checkProfileeCommand :: FilePath -> IO FilePath
checkProfileeCommand profileeCommand = do
   let (profileeCommandDir, _profileeCommandFile) = splitFileName profileeCommand
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
   { options_events :: [String]
   , options_output :: FilePath
   , options_help :: Bool
   } deriving Show

defaultOptions :: Options
defaultOptions = Options
   { options_events = []
   , options_output = defaultPerfOutputFile
   , options_help = False
   }

defaultPerfOutputFile :: FilePath
defaultPerfOutputFile = "perf.data"

-- parse the command line arguments that appeared between +RefPerf and -RecPerf
parseRecPerfOptions :: [String] -> IO Options
parseRecPerfOptions argv =
   case getOpt Permute options argv of
      (foundOptions, _unknowns, _errors@[]) -> do
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
           "e" ["event"]
           (ReqArg (\e opts -> opts { options_events = e : options_events opts }) "E")
           "Event to trace, instead of default events. Can be specified multiple times."

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
-- line. Return two lists: 1) everything between the markers,
-- and 2) everything else.
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

-- Attach "perf record" to the profilee process ID.
perfProcess :: Options -> FilePath -> [String] -> IO ()
perfProcess options program pArgs = do
   executeFile "perf" True (command ++ args) Nothing
   where
   command = ["record"]
   args = concat [output, frequency, selectedEvents, profilee]
   output = ["-o", options_output options]
   frequency = ["-c", "1"]
   profilee = program : pArgs
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
    "syscalls:sys_exit_gettimeofday"
   ]

-- Given two lists [a, b, c ..] [d, e, f ..]
-- return a single list by alternating elements from
-- each: [a, d, b, e, c, f ..] until at least one
-- of the lists is exhausted.
alternate :: [a] -> [a] -> [a]
alternate [] _ = []
alternate _ [] = []
alternate (x:xs) (y:ys) = x : y : alternate xs ys
