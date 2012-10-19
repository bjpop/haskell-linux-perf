-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2012 Bernie Pope, Mikolaj Konarski
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- A tool to "perf record" a trace of another program.
--
-- The linux performance counter tool "perf" can record events
-- for a given program. This tool runs "perf", adding our default set
-- of options. In particular, it specifies our default set
-- of events to be recorded.
--
-- Usage:
-- ghc-events-perf-record [--RTS]
--          [ +GhcEventsPerf [record-args ... ] -GhcEventsPerf ]
--          program-name [program-args ... ]
--
-- To get help:
-- ghc-events-perf-record +GhcEventsPerf -h
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

main :: IO ()
main = getArgs >>= command

command :: [String] -> IO ()
command argv = do
   let (recPerfArgv, profileeArgv) = grabGhcEventsPerfArgv argv
   recPerfOptions <- parseGhcEventsPerfOptions recPerfArgv
   profileCommand recPerfOptions profileeArgv

profileCommand :: Options -> [String] -> IO ()
profileCommand _options [] = ioError $ userError ("You did not supply a name of a program to profile")
profileCommand options (profileeCommand:profileeArgs) = do
   profileePath <- checkProfileeCommand profileeCommand
   -- run perf record with the profilee program
   perfProcess options profileePath profileeArgs

-- Check if the profilee program exists and is executable.
checkProfileeCommand :: FilePath -> IO FilePath
checkProfileeCommand profileeCommand = do
   let (profileeCommandDir, _profileeCommandFile) = splitFileName profileeCommand
   profileePath <-
      if null profileeCommandDir
         -- profilee program name was not prefixed with a directory path
         then do
            -- try to look up the program name in the PATH environment
            maybeProfileePath <- findExecutable profileeCommand
            case maybeProfileePath of
               Nothing -> ioError $ userError ("Command: " ++ profileeCommand ++ " not found")
               Just profileePath -> return profileePath
      else
         return profileeCommand
   exists <- doesFileExist profileePath
   if exists
      then do
         -- check if we can execute the program
         perms <- getPermissions profileePath
         if executable perms
            then return profileePath
            else ioError $ userError ("You do not have permission to execute program: " ++ profileePath)
      else ioError $ userError ("File: " ++ profileePath ++ " does not exist")

-- Options for ghc-events-perf-record, some of which are passed on
-- to "perf record"
data Options = Options
   { options_events :: [String]
   , options_mmap :: String
   , options_output :: FilePath
   , options_help :: Bool
   } deriving Show

defaultOptions :: Options
defaultOptions = Options
   { options_events = []
   , options_mmap = ""
   , options_output = defaultPerfOutputFile
   , options_help = False
   }

defaultPerfOutputFile :: FilePath
defaultPerfOutputFile = "perf.data"

-- parse the command line arguments that appeared between +RefPerf and -GhcEventsPerf
parseGhcEventsPerfOptions :: [String] -> IO Options
parseGhcEventsPerfOptions argv =
   case getOpt Permute recOptions argv of
      (foundOptions, _unknowns, _errors@[]) -> do
         let options = foldl (flip id) defaultOptions foundOptions
         if options_help options
            then putStrLn usage >> exitSuccess
            else return options
      (_flags, _unknowns , errs@(_:_)) ->
         ioError $ userError (concat errs ++ usage)

usage :: String
usage = usageInfo header recOptions

header :: String
header = "Usage: ghc-events-perf-record [--RTS] [ +GhcEventsPerf [record-args ... ] -GhcEventsPerf ] program-name [program-args ... ]"

recOptions :: [OptDescr (Options -> Options)]
recOptions =
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
      , Option
           "m" ["mmap-pages"]
           (ReqArg (\mm opts -> opts {options_mmap = mm }) "MM")
           ("Number of mmap data pages. " ++
           "Defaults to " ++ defaultMmap ++ ".")
      ]

-- Check that a string can be safely intepreted as an integer,
-- otherwise fail with an error message.
safeReadInt :: String -> String -> Int
safeReadInt option cs
   | length cs > 0 && all isDigit cs = read cs
   | otherwise =
        error ("The argument for option " ++ option ++
               " should be an integer, but it was " ++ cs)

-- Cut out all the arguments between +GhcEventsPerf -GhcEventsPerf from the command
-- line. Return two lists: 1) everything between the markers,
-- and 2) everything else.
grabGhcEventsPerfArgv :: [String] -> ([String], [String])
grabGhcEventsPerfArgv cmdline =
   (reverse cmdIns, reverse cmdOuts)
   where
   (cmdIns, cmdOuts) = outside cmdline ([], [])
   outside, inside :: [String] -> ([String], [String]) -> ([String], [String])
   outside [] acc = acc
   outside ("+GhcEventsPerf":rest) acc = inside rest acc
   outside (str:rest) (ins, outs) = outside rest (ins, str:outs)
   inside [] acc = acc
   inside ("-GhcEventsPerf":rest) acc = outside rest acc
   inside (str:rest) (ins, outs) = inside rest (str:ins, outs)

-- Run "perf record" with our options and the profilee program.
perfProcess :: Options -> FilePath -> [String] -> IO ()
perfProcess options program pArgs = do
   executeFile "perf" True (perfCommand ++ args) Nothing
   where
   perfCommand = ["record"]
   args =
     concat [output, frequency, moreTimestamps, mmap, selectedEvents, profilee]
   output = ["-o", options_output options]
   -- Value 1 means hIgh frequency, needed for tracepoints,
   -- but too much traffic for counters/
   frequency = ["-c", "1"]
   -- From linux-perf-users@vger.kernel.org:
   -- The "--timestamp" option adds the timestamps to the samples
   -- if it were not otherwise added.
   moreTimestamps = ["--timestamp"]
   profilee = program : pArgs
   -- If no events were specified on the command line then use the defaults.
   selectedEvents
      | null optionEvents = mkEventFlags defaultEvents
      | otherwise = mkEventFlags optionEvents
      where
      optionEvents = options_events options
   -- If no mmap pages were specified on the command line, use the defaults.
   selectedMmap
      | null optionMmap = defaultMmap
      | otherwise = optionMmap
      where
      optionMmap = options_mmap options
   mmap = ["--mmap-pages", selectedMmap]
   mkEventFlags :: [String] -> [String]
   mkEventFlags = alternate (repeat "-e")

-- The default value of the mmap-pages setting.
-- That's an order of magnitude too little to avoid IO/CPU overload
-- with typical examples, but that's the highest permitted value,
-- unless ghc-events-perf-record is run as root. Can be overridden by the user.
defaultMmap :: String
defaultMmap = "128"

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
    "sched:sched_stat_iowait",
    "sched:sched_pi_setprio",
-- These are too frequent, mostly in COMM events, and cause IO/CPU overload:
--  "sched:sched_stat_wait",
--  "sched:sched_stat_sleep",
--  "sched:sched_stat_runtime",

    -- system calls to record
    "raw_syscalls:sys_enter",
    "raw_syscalls:sys_exit"
   ]

-- Given two lists [a, b, c ..] [d, e, f ..]
-- return a single list by alternating elements from
-- each: [a, d, b, e, c, f ..] until at least one
-- of the lists is exhausted.
alternate :: [a] -> [a] -> [a]
alternate [] _ = []
alternate _ [] = []
alternate (x:xs) (y:ys) = x : y : alternate xs ys
