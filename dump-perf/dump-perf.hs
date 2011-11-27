import Profiling.Linux.Perf
import System.Exit
import System.IO
import System.Environment

main = do
  args <- getArgs
  file <- case args of
            []     -> return "perf.data"
            [file] -> return file
            _      -> die "Syntax: dump-perf [file]"
  readEventsFromFile file

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)
