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
   -- profilee = "/home/bjpop/.cabal/bin/sleep_run" : (program : pArgs)
   profilee = "sleep_run" : (program : pArgs)
{-
     "--" : "/bin/bash" : "-c" :
     [perlHack ++ ";" ++ unwords (program : pArgs)]
   perlHack = "perl -MTime::HiRes -e 'Time::HiRes::nanosleep 3'"
-}
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
    "sched:sched_pi_setprio"
-- These are too frequent, mostly in COMM events, and cause IO/CPU overload:
--  "sched:sched_stat_wait",
--  "sched:sched_stat_sleep",
--  "sched:sched_stat_runtime",

    -- system calls to record
-- For raw_syscalls, we currently don't indicate which system call is made.
-- These events are usually too dense, too.
--  "raw_syscalls:sys_enter",
--  "raw_syscalls:sys_exit"
   ] ++ syscalls3 ++ syscalls4 ++ syscalls5 ++ syscalls6 ++ syscalls7
     ++ syscalls8 ++ syscalls9 ++ syscalls10 ++ syscalls11
-- Tracing more syscalls causes
-- Error: sys_perf_event_open() syscall returned with 24 (Too many open files)
--   ++ syscalls1 ++ syscalls2
  where
  -- The list taken from http://www.berniepope.id.au/linuxPerfEvents.html.
  syscalls1 =
   [
    "syscalls:sys_enter_socket",
    "syscalls:sys_exit_socket",
    "syscalls:sys_enter_socketpair",
    "syscalls:sys_exit_socketpair",
    "syscalls:sys_enter_bind",
    "syscalls:sys_exit_bind",
    "syscalls:sys_enter_listen",
    "syscalls:sys_exit_listen",
    "syscalls:sys_enter_accept4",
    "syscalls:sys_exit_accept4",
    "syscalls:sys_enter_accept",
    "syscalls:sys_exit_accept",
    "syscalls:sys_enter_connect",
    "syscalls:sys_exit_connect",
    "syscalls:sys_enter_getsockname",
    "syscalls:sys_exit_getsockname",
    "syscalls:sys_enter_getpeername",
    "syscalls:sys_exit_getpeername",
    "syscalls:sys_enter_sendto",
    "syscalls:sys_exit_sendto",
    "syscalls:sys_enter_recvfrom",
    "syscalls:sys_exit_recvfrom",
    "syscalls:sys_enter_setsockopt",
    "syscalls:sys_exit_setsockopt",
    "syscalls:sys_enter_getsockopt",
    "syscalls:sys_exit_getsockopt",
    "syscalls:sys_enter_shutdown",
    "syscalls:sys_exit_shutdown",
    "syscalls:sys_enter_sendmsg",
    "syscalls:sys_exit_sendmsg",
    "syscalls:sys_enter_sendmmsg",
    "syscalls:sys_exit_sendmmsg",
    "syscalls:sys_enter_recvmsg",
    "syscalls:sys_exit_recvmsg",
    "syscalls:sys_enter_recvmmsg",
    "syscalls:sys_exit_recvmmsg",
    "syscalls:sys_enter_add_key",
    "syscalls:sys_exit_add_key",
    "syscalls:sys_enter_request_key",
    "syscalls:sys_exit_request_key",
    "syscalls:sys_enter_keyctl",
    "syscalls:sys_exit_keyctl",
    "syscalls:sys_enter_mq_open",
    "syscalls:sys_exit_mq_open",
    "syscalls:sys_enter_mq_unlink",
    "syscalls:sys_exit_mq_unlink",
    "syscalls:sys_enter_mq_timedsend",
    "syscalls:sys_exit_mq_timedsend",
    "syscalls:sys_enter_mq_timedreceive",
    "syscalls:sys_exit_mq_timedreceive"
   ]
  syscalls2 =
   [
    "syscalls:sys_enter_mq_notify",
    "syscalls:sys_exit_mq_notify",
    "syscalls:sys_enter_mq_getsetattr",
    "syscalls:sys_exit_mq_getsetattr",
    "syscalls:sys_enter_shmget",
    "syscalls:sys_exit_shmget",
    "syscalls:sys_enter_shmctl",
    "syscalls:sys_exit_shmctl",
    "syscalls:sys_enter_shmat",
    "syscalls:sys_exit_shmat",
    "syscalls:sys_enter_shmdt",
    "syscalls:sys_exit_shmdt",
    "syscalls:sys_enter_semget",
    "syscalls:sys_exit_semget",
    "syscalls:sys_enter_semtimedop",
    "syscalls:sys_exit_semtimedop",
    "syscalls:sys_enter_semop",
    "syscalls:sys_exit_semop",
    "syscalls:sys_enter_msgget",
    "syscalls:sys_exit_msgget",
    "syscalls:sys_enter_msgctl",
    "syscalls:sys_exit_msgctl",
    "syscalls:sys_enter_msgsnd",
    "syscalls:sys_exit_msgsnd",
    "syscalls:sys_enter_msgrcv",
    "syscalls:sys_exit_msgrcv",
    "syscalls:sys_enter_quotactl",
    "syscalls:sys_exit_quotactl",
    "syscalls:sys_enter_name_to_handle_at",
    "syscalls:sys_exit_name_to_handle_at",
    "syscalls:sys_enter_open_by_handle_at",
    "syscalls:sys_exit_open_by_handle_at",
-- Not present on some systems
--    "syscalls:sys_enter_nfsservctl",
--    "syscalls:sys_exit_nfsservctl",
    "syscalls:sys_enter_flock",
    "syscalls:sys_exit_flock",
    "syscalls:sys_enter_io_setup",
    "syscalls:sys_exit_io_setup",
    "syscalls:sys_enter_io_destroy",
    "syscalls:sys_exit_io_destroy",
    "syscalls:sys_enter_io_submit",
    "syscalls:sys_exit_io_submit",
    "syscalls:sys_enter_io_cancel",
    "syscalls:sys_exit_io_cancel",
    "syscalls:sys_enter_io_getevents",
    "syscalls:sys_exit_io_getevents",
    "syscalls:sys_enter_eventfd2",
    "syscalls:sys_exit_eventfd2",
    "syscalls:sys_enter_eventfd",
    "syscalls:sys_exit_eventfd"
   ]
  syscalls3 =
   [
    "syscalls:sys_enter_timerfd_create",
    "syscalls:sys_exit_timerfd_create",
    "syscalls:sys_enter_timerfd_settime",
    "syscalls:sys_exit_timerfd_settime",
    "syscalls:sys_enter_timerfd_gettime",
    "syscalls:sys_exit_timerfd_gettime",
    "syscalls:sys_enter_signalfd4",
    "syscalls:sys_exit_signalfd4",
    "syscalls:sys_enter_signalfd",
    "syscalls:sys_exit_signalfd",
    "syscalls:sys_enter_epoll_create1",
    "syscalls:sys_exit_epoll_create1",
    "syscalls:sys_enter_epoll_create",
    "syscalls:sys_exit_epoll_create",
    "syscalls:sys_enter_epoll_ctl",
    "syscalls:sys_exit_epoll_ctl",
    "syscalls:sys_enter_epoll_wait",
    "syscalls:sys_exit_epoll_wait",
    "syscalls:sys_enter_epoll_pwait",
    "syscalls:sys_exit_epoll_pwait",
    "syscalls:sys_enter_fanotify_init",
    "syscalls:sys_exit_fanotify_init",
    "syscalls:sys_enter_inotify_init1",
    "syscalls:sys_exit_inotify_init1",
    "syscalls:sys_enter_inotify_init",
    "syscalls:sys_exit_inotify_init",
    "syscalls:sys_enter_inotify_add_watch",
    "syscalls:sys_exit_inotify_add_watch",
    "syscalls:sys_enter_inotify_rm_watch",
    "syscalls:sys_exit_inotify_rm_watch",
    "syscalls:sys_enter_ioprio_set",
    "syscalls:sys_exit_ioprio_set",
    "syscalls:sys_enter_ioprio_get",
    "syscalls:sys_exit_ioprio_get",
    "syscalls:sys_enter_statfs",
    "syscalls:sys_exit_statfs",
    "syscalls:sys_enter_fstatfs",
    "syscalls:sys_exit_fstatfs",
    "syscalls:sys_enter_ustat",
    "syscalls:sys_exit_ustat",
    "syscalls:sys_enter_utime",
    "syscalls:sys_exit_utime",
    "syscalls:sys_enter_utimensat",
    "syscalls:sys_exit_utimensat",
    "syscalls:sys_enter_futimesat",
    "syscalls:sys_exit_futimesat",
    "syscalls:sys_enter_utimes",
    "syscalls:sys_exit_utimes",
    "syscalls:sys_enter_sync",
    "syscalls:sys_exit_sync"
   ]
  syscalls4 =
   [
    "syscalls:sys_enter_syncfs",
    "syscalls:sys_exit_syncfs",
    "syscalls:sys_enter_fsync",
    "syscalls:sys_exit_fsync",
    "syscalls:sys_enter_fdatasync",
    "syscalls:sys_exit_fdatasync",
    "syscalls:sys_enter_vmsplice",
    "syscalls:sys_exit_vmsplice",
    "syscalls:sys_enter_splice",
    "syscalls:sys_exit_splice",
    "syscalls:sys_enter_tee",
    "syscalls:sys_exit_tee",
    "syscalls:sys_enter_setxattr",
    "syscalls:sys_exit_setxattr",
    "syscalls:sys_enter_lsetxattr",
    "syscalls:sys_exit_lsetxattr",
    "syscalls:sys_enter_fsetxattr",
    "syscalls:sys_exit_fsetxattr",
    "syscalls:sys_enter_getxattr",
    "syscalls:sys_exit_getxattr",
    "syscalls:sys_enter_lgetxattr",
    "syscalls:sys_exit_lgetxattr",
    "syscalls:sys_enter_fgetxattr",
    "syscalls:sys_exit_fgetxattr",
    "syscalls:sys_enter_listxattr",
    "syscalls:sys_exit_listxattr",
    "syscalls:sys_enter_llistxattr",
    "syscalls:sys_exit_llistxattr",
    "syscalls:sys_enter_flistxattr",
    "syscalls:sys_exit_flistxattr",
    "syscalls:sys_enter_removexattr",
    "syscalls:sys_exit_removexattr",
    "syscalls:sys_enter_lremovexattr",
    "syscalls:sys_exit_lremovexattr",
    "syscalls:sys_enter_fremovexattr",
    "syscalls:sys_exit_fremovexattr",
    "syscalls:sys_enter_umount",
    "syscalls:sys_exit_umount",
    "syscalls:sys_enter_mount",
    "syscalls:sys_exit_mount",
    "syscalls:sys_enter_pivot_root",
    "syscalls:sys_exit_pivot_root",
    "syscalls:sys_enter_sysfs",
    "syscalls:sys_exit_sysfs",
    "syscalls:sys_enter_getcwd",
    "syscalls:sys_exit_getcwd",
    "syscalls:sys_enter_select",
    "syscalls:sys_exit_select",
    "syscalls:sys_enter_pselect6",
    "syscalls:sys_exit_pselect6"
   ]
  syscalls5 =
   [
    "syscalls:sys_enter_poll",
    "syscalls:sys_exit_poll",
    "syscalls:sys_enter_ppoll",
    "syscalls:sys_exit_ppoll",
    "syscalls:sys_enter_getdents",
    "syscalls:sys_exit_getdents",
    "syscalls:sys_enter_getdents64",
    "syscalls:sys_exit_getdents64",
    "syscalls:sys_enter_ioctl",
    "syscalls:sys_exit_ioctl",
    "syscalls:sys_enter_dup3",
    "syscalls:sys_exit_dup3",
    "syscalls:sys_enter_dup2",
    "syscalls:sys_exit_dup2",
    "syscalls:sys_enter_dup",
    "syscalls:sys_exit_dup",
    "syscalls:sys_enter_fcntl",
    "syscalls:sys_exit_fcntl",
    "syscalls:sys_enter_mknodat",
    "syscalls:sys_exit_mknodat",
    "syscalls:sys_enter_mknod",
    "syscalls:sys_exit_mknod",
    "syscalls:sys_enter_mkdirat",
    "syscalls:sys_exit_mkdirat",
    "syscalls:sys_enter_mkdir",
    "syscalls:sys_exit_mkdir",
    "syscalls:sys_enter_rmdir",
    "syscalls:sys_exit_rmdir",
    "syscalls:sys_enter_unlinkat",
    "syscalls:sys_exit_unlinkat",
    "syscalls:sys_enter_unlink",
    "syscalls:sys_exit_unlink",
    "syscalls:sys_enter_symlinkat",
    "syscalls:sys_exit_symlinkat",
    "syscalls:sys_enter_symlink",
    "syscalls:sys_exit_symlink",
    "syscalls:sys_enter_linkat",
    "syscalls:sys_exit_linkat",
    "syscalls:sys_enter_link",
    "syscalls:sys_exit_link",
    "syscalls:sys_enter_renameat",
    "syscalls:sys_exit_renameat",
    "syscalls:sys_enter_rename",
    "syscalls:sys_exit_rename",
    "syscalls:sys_enter_pipe2",
    "syscalls:sys_exit_pipe2",
    "syscalls:sys_enter_pipe",
    "syscalls:sys_exit_pipe"
   ]
  syscalls6 =
   [
    "syscalls:sys_enter_newstat",
    "syscalls:sys_exit_newstat",
    "syscalls:sys_enter_newlstat",
    "syscalls:sys_exit_newlstat",
    "syscalls:sys_enter_newfstatat",
    "syscalls:sys_exit_newfstatat",
    "syscalls:sys_enter_newfstat",
    "syscalls:sys_exit_newfstat",
    "syscalls:sys_enter_readlinkat",
    "syscalls:sys_exit_readlinkat",
    "syscalls:sys_enter_readlink",
    "syscalls:sys_exit_readlink",
    "syscalls:sys_enter_lseek",
    "syscalls:sys_exit_lseek",
    "syscalls:sys_enter_read",
    "syscalls:sys_exit_read",
    "syscalls:sys_enter_write",
    "syscalls:sys_exit_write",
    "syscalls:sys_enter_readv",
    "syscalls:sys_exit_readv",
    "syscalls:sys_enter_writev",
    "syscalls:sys_exit_writev",
    "syscalls:sys_enter_preadv",
    "syscalls:sys_exit_preadv",
    "syscalls:sys_enter_pwritev",
    "syscalls:sys_exit_pwritev",
    "syscalls:sys_enter_sendfile64",
    "syscalls:sys_exit_sendfile64",
    "syscalls:sys_enter_truncate",
    "syscalls:sys_exit_truncate",
    "syscalls:sys_enter_ftruncate",
    "syscalls:sys_exit_ftruncate",
    "syscalls:sys_enter_faccessat",
    "syscalls:sys_exit_faccessat",
    "syscalls:sys_enter_access",
    "syscalls:sys_exit_access",
    "syscalls:sys_enter_chdir",
    "syscalls:sys_exit_chdir",
    "syscalls:sys_enter_fchdir",
    "syscalls:sys_exit_fchdir",
    "syscalls:sys_enter_chroot",
    "syscalls:sys_exit_chroot",
    "syscalls:sys_enter_fchmod",
    "syscalls:sys_exit_fchmod",
    "syscalls:sys_enter_fchmodat",
    "syscalls:sys_exit_fchmodat",
    "syscalls:sys_enter_chmod",
    "syscalls:sys_exit_chmod",
    "syscalls:sys_enter_chown",
    "syscalls:sys_exit_chown",
    "syscalls:sys_enter_fchownat",
    "syscalls:sys_exit_fchownat"
   ]
  syscalls7 =
   [
    "syscalls:sys_enter_lchown",
    "syscalls:sys_exit_lchown",
    "syscalls:sys_enter_fchown",
    "syscalls:sys_exit_fchown",
    "syscalls:sys_enter_open",
    "syscalls:sys_exit_open",
    "syscalls:sys_enter_openat",
    "syscalls:sys_exit_openat",
    "syscalls:sys_enter_creat",
    "syscalls:sys_exit_creat",
    "syscalls:sys_enter_close",
    "syscalls:sys_exit_close",
    "syscalls:sys_enter_vhangup",
    "syscalls:sys_exit_vhangup",
    "syscalls:sys_enter_move_pages",
    "syscalls:sys_exit_move_pages",
    "syscalls:sys_enter_mbind",
    "syscalls:sys_exit_mbind",
    "syscalls:sys_enter_set_mempolicy",
    "syscalls:sys_exit_set_mempolicy",
    "syscalls:sys_enter_migrate_pages",
    "syscalls:sys_exit_migrate_pages",
    "syscalls:sys_enter_get_mempolicy",
    "syscalls:sys_exit_get_mempolicy",
    "syscalls:sys_enter_swapoff",
    "syscalls:sys_exit_swapoff",
    "syscalls:sys_enter_swapon",
    "syscalls:sys_exit_swapon",
    "syscalls:sys_enter_msync",
    "syscalls:sys_exit_msync",
    "syscalls:sys_enter_mremap",
    "syscalls:sys_exit_mremap",
    "syscalls:sys_enter_mprotect",
    "syscalls:sys_exit_mprotect",
    "syscalls:sys_enter_brk",
    "syscalls:sys_exit_brk",
    "syscalls:sys_enter_munmap",
    "syscalls:sys_exit_munmap",
    "syscalls:sys_enter_mlock",
    "syscalls:sys_exit_mlock",
    "syscalls:sys_enter_munlock",
    "syscalls:sys_exit_munlock",
    "syscalls:sys_enter_mlockall",
    "syscalls:sys_exit_mlockall",
    "syscalls:sys_enter_munlockall",
    "syscalls:sys_exit_munlockall",
    "syscalls:sys_enter_mincore",
    "syscalls:sys_exit_mincore",
    "syscalls:sys_enter_madvise",
    "syscalls:sys_exit_madvise",
    "syscalls:sys_enter_remap_file_pages",
    "syscalls:sys_exit_remap_file_pages"
   ]
  syscalls8 =
   [
    "syscalls:sys_enter_perf_event_open",
    "syscalls:sys_exit_perf_event_open",
    "syscalls:sys_enter_kexec_load",
    "syscalls:sys_exit_kexec_load",
    "syscalls:sys_enter_acct",
    "syscalls:sys_exit_acct",
    "syscalls:sys_enter_delete_module",
    "syscalls:sys_exit_delete_module",
    "syscalls:sys_enter_init_module",
    "syscalls:sys_exit_init_module",
    "syscalls:sys_enter_set_robust_list",
    "syscalls:sys_exit_set_robust_list",
    "syscalls:sys_enter_get_robust_list",
    "syscalls:sys_exit_get_robust_list",
    "syscalls:sys_enter_futex",
    "syscalls:sys_exit_futex",
    "syscalls:sys_enter_getgroups",
    "syscalls:sys_exit_getgroups",
    "syscalls:sys_enter_setgroups",
    "syscalls:sys_exit_setgroups",
    "syscalls:sys_enter_setns",
    "syscalls:sys_exit_setns",
    "syscalls:sys_enter_nanosleep",
    "syscalls:sys_exit_nanosleep",
    "syscalls:sys_enter_timer_create",
    "syscalls:sys_exit_timer_create",
    "syscalls:sys_enter_timer_gettime",
    "syscalls:sys_exit_timer_gettime",
    "syscalls:sys_enter_timer_getoverrun",
    "syscalls:sys_exit_timer_getoverrun",
    "syscalls:sys_enter_timer_settime",
    "syscalls:sys_exit_timer_settime",
    "syscalls:sys_enter_timer_delete",
    "syscalls:sys_exit_timer_delete",
    "syscalls:sys_enter_clock_settime",
    "syscalls:sys_exit_clock_settime",
-- Too common, crowds the display
--    "syscalls:sys_enter_clock_gettime",
--    "syscalls:sys_exit_clock_gettime",
    "syscalls:sys_enter_clock_adjtime",
    "syscalls:sys_exit_clock_adjtime",
    "syscalls:sys_enter_clock_getres",
    "syscalls:sys_exit_clock_getres",
    "syscalls:sys_enter_clock_nanosleep",
    "syscalls:sys_exit_clock_nanosleep",
    "syscalls:sys_enter_setpriority",
    "syscalls:sys_exit_setpriority",
    "syscalls:sys_enter_getpriority",
    "syscalls:sys_exit_getpriority",
    "syscalls:sys_enter_reboot",
    "syscalls:sys_exit_reboot",
    "syscalls:sys_enter_setregid",
    "syscalls:sys_exit_setregid"
   ]
  syscalls9 =
   [
    "syscalls:sys_enter_setgid",
    "syscalls:sys_exit_setgid",
    "syscalls:sys_enter_setreuid",
    "syscalls:sys_exit_setreuid",
    "syscalls:sys_enter_setuid",
    "syscalls:sys_exit_setuid",
    "syscalls:sys_enter_setresuid",
    "syscalls:sys_exit_setresuid",
    "syscalls:sys_enter_getresuid",
    "syscalls:sys_exit_getresuid",
    "syscalls:sys_enter_setresgid",
    "syscalls:sys_exit_setresgid",
    "syscalls:sys_enter_getresgid",
    "syscalls:sys_exit_getresgid",
    "syscalls:sys_enter_setfsuid",
    "syscalls:sys_exit_setfsuid",
    "syscalls:sys_enter_setfsgid",
    "syscalls:sys_exit_setfsgid",
    "syscalls:sys_enter_times",
    "syscalls:sys_exit_times",
    "syscalls:sys_enter_setpgid",
    "syscalls:sys_exit_setpgid",
    "syscalls:sys_enter_getpgid",
    "syscalls:sys_exit_getpgid",
    "syscalls:sys_enter_getpgrp",
    "syscalls:sys_exit_getpgrp",
    "syscalls:sys_enter_getsid",
    "syscalls:sys_exit_getsid",
    "syscalls:sys_enter_setsid",
    "syscalls:sys_exit_setsid",
    "syscalls:sys_enter_newuname",
    "syscalls:sys_exit_newuname",
    "syscalls:sys_enter_sethostname",
    "syscalls:sys_exit_sethostname",
    "syscalls:sys_enter_setdomainname",
    "syscalls:sys_exit_setdomainname",
    "syscalls:sys_enter_getrlimit",
    "syscalls:sys_exit_getrlimit",
    "syscalls:sys_enter_prlimit64",
    "syscalls:sys_exit_prlimit64",
    "syscalls:sys_enter_setrlimit",
    "syscalls:sys_exit_setrlimit",
    "syscalls:sys_enter_getrusage",
    "syscalls:sys_exit_getrusage",
    "syscalls:sys_enter_umask",
    "syscalls:sys_exit_umask",
    "syscalls:sys_enter_prctl",
    "syscalls:sys_exit_prctl",
    "syscalls:sys_enter_restart_syscall",
    "syscalls:sys_exit_restart_syscall"
   ]
  syscalls10 =
   [
    "syscalls:sys_enter_rt_sigprocmask",
    "syscalls:sys_exit_rt_sigprocmask",
    "syscalls:sys_enter_rt_sigpending",
    "syscalls:sys_exit_rt_sigpending",
    "syscalls:sys_enter_rt_sigtimedwait",
    "syscalls:sys_exit_rt_sigtimedwait",
    "syscalls:sys_enter_kill",
    "syscalls:sys_exit_kill",
    "syscalls:sys_enter_tgkill",
    "syscalls:sys_exit_tgkill",
    "syscalls:sys_enter_tkill",
    "syscalls:sys_exit_tkill",
    "syscalls:sys_enter_rt_sigqueueinfo",
    "syscalls:sys_exit_rt_sigqueueinfo",
    "syscalls:sys_enter_rt_tgsigqueueinfo",
    "syscalls:sys_exit_rt_tgsigqueueinfo",
    "syscalls:sys_enter_rt_sigaction",
    "syscalls:sys_exit_rt_sigaction",
    "syscalls:sys_enter_pause",
    "syscalls:sys_exit_pause",
    "syscalls:sys_enter_rt_sigsuspend",
    "syscalls:sys_exit_rt_sigsuspend",
    "syscalls:sys_enter_alarm",
    "syscalls:sys_exit_alarm",
    "syscalls:sys_enter_getpid",
    "syscalls:sys_exit_getpid",
    "syscalls:sys_enter_getppid",
    "syscalls:sys_exit_getppid",
    "syscalls:sys_enter_getuid",
    "syscalls:sys_exit_getuid",
    "syscalls:sys_enter_geteuid",
    "syscalls:sys_exit_geteuid",
    "syscalls:sys_enter_getgid",
    "syscalls:sys_exit_getgid",
    "syscalls:sys_enter_getegid",
    "syscalls:sys_exit_getegid",
    "syscalls:sys_enter_gettid",
    "syscalls:sys_exit_gettid",
    "syscalls:sys_enter_sysinfo",
    "syscalls:sys_exit_sysinfo",
    "syscalls:sys_enter_ptrace",
    "syscalls:sys_exit_ptrace",
    "syscalls:sys_enter_capget",
    "syscalls:sys_exit_capget",
    "syscalls:sys_enter_capset",
    "syscalls:sys_exit_capset",
    "syscalls:sys_enter_sysctl",
    "syscalls:sys_exit_sysctl"
   ]
  syscalls11 =
   [
    "syscalls:sys_enter_time",
    "syscalls:sys_exit_time",
    "syscalls:sys_enter_gettimeofday",
    "syscalls:sys_exit_gettimeofday",
    "syscalls:sys_enter_settimeofday",
    "syscalls:sys_exit_settimeofday",
    "syscalls:sys_enter_adjtimex",
    "syscalls:sys_exit_adjtimex",
    "syscalls:sys_enter_getitimer",
    "syscalls:sys_exit_getitimer",
    "syscalls:sys_enter_setitimer",
    "syscalls:sys_exit_setitimer",
    "syscalls:sys_enter_exit",
    "syscalls:sys_exit_exit",
    "syscalls:sys_enter_exit_group",
    "syscalls:sys_exit_exit_group",
    "syscalls:sys_enter_waitid",
    "syscalls:sys_exit_waitid",
    "syscalls:sys_enter_wait4",
    "syscalls:sys_exit_wait4",
    "syscalls:sys_enter_syslog",
    "syscalls:sys_exit_syslog",
    "syscalls:sys_enter_personality",
    "syscalls:sys_exit_personality",
    "syscalls:sys_enter_set_tid_address",
    "syscalls:sys_exit_set_tid_address",
    "syscalls:sys_enter_unshare",
    "syscalls:sys_exit_unshare",
    "syscalls:sys_enter_sched_setscheduler",
    "syscalls:sys_exit_sched_setscheduler",
    "syscalls:sys_enter_sched_setparam",
    "syscalls:sys_exit_sched_setparam",
    "syscalls:sys_enter_sched_getscheduler",
    "syscalls:sys_exit_sched_getscheduler",
    "syscalls:sys_enter_sched_getparam",
    "syscalls:sys_exit_sched_getparam",
    "syscalls:sys_enter_sched_setaffinity",
    "syscalls:sys_exit_sched_setaffinity",
    "syscalls:sys_enter_sched_getaffinity",
    "syscalls:sys_exit_sched_getaffinity",
-- Too common, crowds the display
--    "syscalls:sys_enter_sched_yield",
--    "syscalls:sys_exit_sched_yield",
    "syscalls:sys_enter_sched_get_priority_max",
    "syscalls:sys_exit_sched_get_priority_max",
    "syscalls:sys_enter_sched_get_priority_min",
    "syscalls:sys_exit_sched_get_priority_min",
    "syscalls:sys_enter_sched_rr_get_interval",
    "syscalls:sys_exit_sched_rr_get_interval",
    "syscalls:sys_enter_mmap",
    "syscalls:sys_exit_mmap"
   ]

-- Given two lists [a, b, c ..] [d, e, f ..]
-- return a single list by alternating elements from
-- each: [a, d, b, e, c, f ..] until at least one
-- of the lists is exhausted.
alternate :: [a] -> [a] -> [a]
alternate [] _ = []
alternate _ [] = []
alternate (x:xs) (y:ys) = x : y : alternate xs ys
