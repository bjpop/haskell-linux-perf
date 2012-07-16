module Main where

-- use like so:
-- rec-perf program args
-- where program is the name of the program to run,
-- and args are the command line arguments of the program
-- you may need to use sudo

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
      -- start a perf process attached to the (sleeping) child
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
      -- Nothing -> signalProcess killProcess pid
      Nothing -> print "Could not get status for process"
      Just (Exited code) -> return ()
      Just (Terminated signal) -> return ()
      Just (Stopped signal) ->
         -- XXX what to do here?
         return ()

childProcess :: FilePath -> [String] -> IO ()
childProcess command args = do
   -- wait for a short time (0.5 second) to allow perf to start recording this process
   threadDelay 500000
   -- run the command to be profiled
   executeFile command True args Nothing

perfProcess :: ProcessID -> IO ()
perfProcess pid = do
    executeFile "perf" True
       ["record",
        "-a",
        "-c", "1",
        "-p", show pid,
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
        "-e", "raw_syscalls:sys_enter",
        "-e", "raw_syscalls:sys_exit", 
        "-e", "syscalls:sys_enter_gettimeofday",
        "-e", "syscalls:sys_exit_gettimeofday", 
        "sleep", "60"]
       Nothing
