-- A dummy Haskell module to make cabal happy. Cabal requires
-- the Main-is field of an executable to name a Haskell source file.
-- This is annoying if you want to use Cabal to build a C program.
-- So this dummy Haskell file is only here to give us something to name.
-- It seems as though we can work around the issue by giving the -no-hs-main
-- flag to GHC, and then telling Cabal to compile the actual C file we want
-- to use.

-- See: https://github.com/haskell/cabal/issues/497

module Main where

main :: IO ()
main = return ()
