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
-- Usage: rec-perf command [command-args ... ]
--
-- perf will write its output to the file "perf.data".
--
-- TODO:
--    - command line argument to specify how long the dummy sleep process
--      runs.
--    - command line arguments to allow user to specify a set of events to
--      to trace instead of the default ones.
--    - command line argument to get perf to save its output to a specific
--      file.
--    - a way to separate command line arguments for rec-perf from those of
--      the profilee command (probably "--").
--    - decide what to do if we are waiting for a process and it stops
--      instead of terminates/exits.
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

main :: IO ()
main = do
   args <- getArgs
   when (length args > 0) $ do
      -- start a process for the command to be profiled
      childPID <- forkProcess $ childProcess (head args) (tail args)
      -- start a perf process attached to the (temporarily sleeping) child
      perfPID <- forkProcess $ perfProcess childPID
      -- wait for the child to terminate
      waitForProcessTerminate childPID
      -- terminate the perf process
      -- perf seems to respect keyboard signal and flush its buffers
      signalProcess keyboardSignal perfPID
      -- wait for perf to terminate
      waitForProcessTerminate perfPID
      return ()

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

perfProcess :: ProcessID -> IO ()
perfProcess pid = do
    executeFile "perf" True
       ["record",

        -- record on all CPUs

        "-a", 

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
        "-e", "syscalls:sys_exit_gettimeofday", 

        -- dummy process.
        -- Perf terminates if this process terminates.
        -- sleep gives us an upper bound on the
        -- tracing operation, which is probably a
        -- useful safeguard. Very long traces could
        -- generate large files, which are dangerous
        -- if run as root (which is commonly required). 

        "sleep", "60" 

       ]
       Nothing
