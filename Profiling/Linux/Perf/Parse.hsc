{-# LANGUAGE RecordWildCards #-}

module Profiling.Linux.Perf.Parse (
       readEventsFromFile
  ) where

import Profiling.Linux.Perf.Types

import Data.Word
import Data.Binary
import Control.Monad.Error
import System.IO
import Data.ByteString.Lazy as B
import Data.Binary.Get
import Debug.Trace
import Text.Printf

#include <linux/perf_event.h>
#include "perf_file.h"

-- -----------------------------------------------------------------------------

type GetEvents a = ErrorT String Get a

getE :: Binary a => GetEvents a
getE = lift get

getU32 :: GetEvents Word32
getU32 = lift getWord32le

getU64 :: GetEvents Word64
getU64 = lift getWord64le

-- -----------------------------------------------------------------------------

data PerfFileSection
  = PerfFileSection { sec_offset :: Word64,
                      sec_size   :: Word64 }

data PerfFileHeader
  = PerfFileHeader {
                 fh_size          :: Word64,    -- Size of (this) header
                 fh_attr_size     :: Word64,    -- Size of one attribute section
                 fh_attrs_offset  :: Word64,
                 fh_attrs_size    :: Word64,
                 fh_data_offset   :: Word64,
                 fh_data_size     :: Word64,
                 fh_event_offset  :: Word64,
                 fh_event_size    :: Word64,
                 fh_adds_features :: [Word32] }

pERF_MAGIC = 0x454c494646524550 :: Word64 -- "PERFMAGIC"
hEADER_FEAT_BITS = (#const HEADER_FEAT_BITS) :: Int

readFileSection :: GetEvents PerfFileSection
readFileSection = do
    sec_offset <- getU64
    sec_size   <- getU64
    return PerfFileSection{..}

-- attr_size = (#sizeof f_attr)

readFileHeader :: GetEvents PerfFileHeader
readFileHeader = do
    magic       <- getU64
    when (magic /= pERF_MAGIC) $
        throwError "incompatible file format, or not a perf file"
    fh_size        <- getU64
    fh_attr_size   <- getU64
    PerfFileSection fh_attrs_offset fh_attrs_size  <- readFileSection
    PerfFileSection fh_data_offset  fh_data_size   <- readFileSection
    PerfFileSection fh_event_offset fh_event_size  <- readFileSection
    fh_adds_features <- replicateM (hEADER_FEAT_BITS `quot` 32) $ getU32
    return PerfFileHeader{..}

data PerfFileAttr = PerfFileAttr {
    fa_attr :: [Word8], -- for now
    fa_ids_offset :: Word64,
    fa_ids_size   :: Word64
  }

parseAttr :: GetEvents PerfFileAttr
parseAttr = do
  fa_attr <- replicateM (#size struct perf_event_attr) getE
  trace (show fa_attr) $ return ()
  PerfFileSection fa_ids_offset fa_ids_size <- readFileSection
  trace (show fa_ids_offset) $ return ()
  trace (show fa_ids_size) $ return ()
  return PerfFileAttr{..}

readEventsFromFile :: FilePath -> IO (PerfFileHeader, [PerfFileAttr])
readEventsFromFile f = do
    h <- openFile f ReadMode

    b <- B.hGet h (#size struct perf_file_header)
    fh <- case runGet (runErrorT $ readFileHeader) b of
            Left err -> fail err
            Right r  -> return r

    printf "fh_size          = %d\n" $ fh_size fh
    printf "fh_attr_size     = %d\n" $ fh_attr_size fh
    printf "fh_attrs_offset  = %d\n" $ fh_attrs_offset fh
    printf "fh_attrs_size    = %d\n" $ fh_attrs_size fh
    printf "fh_data_offset   = %d\n" $ fh_data_offset fh
    printf "fh_data_size     = %d\n" $ fh_data_size fh
    printf "fh_event_offset  = %d\n" $ fh_event_offset fh
    printf "fh_event_size    = %d\n" $ fh_event_size fh
    printf "fh_adds_features = %s\n" $ (show (fh_adds_features fh))

    let nr_attrs = fh_attrs_size fh `quot` (#size struct perf_file_attr)
    printf "nr_attrs = %d\n" nr_attrs

    printf "size struct perf_file_attr: %d\n" ((#size struct perf_file_attr) :: Int)
    hSeek h AbsoluteSeek (fromIntegral (fh_attrs_offset fh))
    b <- hGet h (fromIntegral (fh_attrs_size fh))
    attrs <- case runGet (runErrorT $ replicateM (fromIntegral nr_attrs)
                                                 parseAttr) b of
               Left err -> fail err
               Right r  -> return r

    return (fh, attrs)
