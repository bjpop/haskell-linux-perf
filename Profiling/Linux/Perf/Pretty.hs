-----------------------------------------------------------------------------
-- |
-- Copyright   : (c) 2012 Simon Marlow, Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Pretty printing utilities.
--
-----------------------------------------------------------------------------

module Profiling.Linux.Perf.Pretty
   ( Pretty (..)
   , prettyString
   , showBits
   )where

import Data.Word (Word64, Word32, Word16, Word8, Word)
import Data.Char (chr)
import Text.PrettyPrint (text, (<+>), ($$), render, empty, integer, (<>), hsep, Doc)
import Data.ByteString.Lazy (ByteString, unpack)
import Data.Bits (testBit, Bits, bitSize)

-- -----------------------------------------------------------------------------
-- Pretty printing interface

class Pretty a where
   pretty :: a -> Doc

prettyString :: Pretty a => a -> String
prettyString = render . pretty

instance Pretty a => Pretty (Maybe a) where
   pretty Nothing = empty
   pretty (Just x) = pretty x

instance Pretty Word8 where
   pretty = integer . fromIntegral

instance Pretty Word16 where
   pretty = integer . fromIntegral

instance Pretty Word32 where
   pretty = integer . fromIntegral

instance Pretty Word64 where
   pretty = integer . fromIntegral

instance (Pretty a, Pretty b) => Pretty (a, b) where
   pretty (x, y) = text "(" <> pretty x <> text "," <+> pretty y <> text ")"

instance Pretty ByteString where
   pretty = text . unpackAsChars
      where
      unpackAsChars :: ByteString -> String
      unpackAsChars bs = foldr (\c cs -> (chr $ fromIntegral c) : cs) [] (unpack bs)

bits :: Bits a => a -> [Bool]
bits x = map (testBit x) [0 .. bitSize x - 1]

showBits :: Bits a => a -> String
showBits = map toBit . bits
   where
   toBit True  = '1'
   toBit False = '0'
