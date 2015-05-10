import Data.Char
import Data.Word

import Data.ByteString.Lazy.Char8 as B

import Data.Attoparsec.ByteString.Lazy as A

testByte :: Word8 -> A.Parser Word8
testByte b = do
    x <- anyWord8
    if x == b then return x else fail "nope"
