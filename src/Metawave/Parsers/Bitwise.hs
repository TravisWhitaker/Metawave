module Metawave.Parsers.Bitwise where

import Data.Word

import Data.ByteString as B

import Data.Binary.Get

import qualified Data.Attoparsec.ByteString as A

oneoff :: Functor f => Get a -> f B.ByteString -> f a
oneoff = fmap . (checkDone .) . pushChunk . runGetIncremental
    where checkDone (Done _ _ w) = w
          checkDone _            = error "Metawave: bitwise parser did not consume all input"

parseWord16be :: A.Parser Word16
parseWord16be = oneoff getWord16be $ A.take 2

parseWord32be :: A.Parser Word32
parseWord32be = oneoff getWord32be $ A.take 4

parseWord64be :: A.Parser Word64
parseWord64be = oneoff getWord64be $ A.take 8

parseWord16le :: A.Parser Word16
parseWord16le = oneoff getWord16le $ A.take 2

parseWord32le :: A.Parser Word32
parseWord32le = oneoff getWord32le $ A.take 4

parseWord64le :: A.Parser Word64
parseWord64le = oneoff getWord64le $ A.take 8
