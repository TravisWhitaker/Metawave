module Metawave.Parsers.Bitwise where

import Data.Word

import Data.ByteString as B

import Data.Binary.Get

import qualified Data.Attoparsec.ByteString as A

oneoff :: Monad m => Get a -> B.ByteString -> m a
oneoff = (ifDone .) . pushChunk . runGetIncremental
    where ifDone (Done _ _ w) = return w
          ifDone _            = fail "bitwise parse fallthrough"

parseWord16be :: A.Parser Word16
parseWord16be = A.take 2 >>= oneoff getWord16be

parseWord32be :: A.Parser Word32
parseWord32be = A.take 4 >>= oneoff getWord32be

parseWord64be :: A.Parser Word64
parseWord64be = A.take 8 >>= oneoff getWord64be

parseWord16le :: A.Parser Word16
parseWord16le = A.take 2 >>= oneoff getWord16le

parseWord32le :: A.Parser Word32
parseWord32le = A.take 4 >>= oneoff getWord32le

parseWord64le :: A.Parser Word64
parseWord64le = A.take 8 >>= oneoff getWord64le
