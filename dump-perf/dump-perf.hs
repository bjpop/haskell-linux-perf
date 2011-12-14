import Profiling.Linux.Perf
import System.Exit
import System.IO
import System.Environment
import Text.Printf
import Text.PrettyPrint

main = do
  args <- getArgs
  file <- case args of
            []     -> return "perf.data"
            [file] -> return file
            _      -> die "Syntax: dump-perf [file]"
  dumper file

die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)

dumper :: FilePath -> IO (PerfFileHeader, [PerfFileAttr])
dumper f = do
   h <- openFile f ReadMode
   header <- readHeader h
   attrs <- readAttributes h header
   idss <- mapM (readAttributeIDs h) attrs
   let separator = printf "%s\n" $ Prelude.replicate 40 '-'
   separator
   printf "Perf File Header:\n"
   separator
   printf "%s\n" $ prettyString header
   separator
   printf "Perf File Attributes:\n"
   separator
   let prettyAttrAndIds (attr, ids) = pretty attr $$ (text "ids:" <+> (hsep $ Prelude.map pretty ids))
   printf "%s\n" $ render $ vcat $ Prelude.map prettyAttrAndIds $ Prelude.zip attrs idss
   eventHeader <- readEventHeader h $ fh_data_offset header
   separator
   printf "Perf Event Header:\n"
   separator
   printf "%s\n" $ prettyString eventHeader
