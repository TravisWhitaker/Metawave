{-# LANGUAGE OverloadedStrings #-}

module Metawave.Parsers.FLAC where

import Data.Bits

import qualified Data.Map.Strict                  as M

import qualified Data.ByteString.Char8            as C

import qualified Data.ByteString.Lazy.Char8       as CL

import Data.Attoparsec.Bytestring.Char8

import Metawave.Parsers.Bitwise

data MetadataBlock = StreamInfo {
    minBlockSamples :: Int
   ,maxBlockSamples :: Int
   ,minFrameBytes   :: Int
   ,maxFrameBytes   :: Int
   ,sampleRate      :: Int
   ,channels        :: Int
   ,bitsPerSample   :: Int
   ,totalSamples    :: Int
   ,md5signature    :: C.ByteString
   }
   | Padding {
    padBytes :: Int
   }
   | Application {
    appID   :: Int
   ,appData :: C.ByteString
   }
   | SeekTable {
    seekpoints :: [Seekpoint]
   }
   | VorbisComments {
    comments :: M.Map C.ByteString C.ByteString
   }
   | Cuesheet {
    catalogNumber :: C.ByteString
   ,leadInSamples :: Int
   ,compactDisc   :: Bool
   ,numTracks     :: Int
   ,tracks        :: [CuesheetTrack]
   }
   | Picture
   {
    apicType   :: APIC
   ,mimeLength :: Int
   ,mimeString :: C.ByteString
   ,descLength :: Int
   ,descString :: C.ByteString
   ,width      :: Int
   ,height     :: Int
   ,colorDepth :: Int
   ,colorIndex :: Int
   ,picLength  :: Int
   ,picture    :: C.ByteString
   }

data Seekpoint = Seekpoint {
    firstSample :: Int
   ,byteOffset  :: Int
   ,sampleSize  :: Int
   }

data CuesheetTrack = CuesheetTrack {
    offsetSamples :: Int
   ,trackNumber   :: Int
   ,isrc          :: C.ByteString
   ,audio         :: Bool
   ,preEmphasis   :: Bool
   ,indices       :: [CuesheetIndex]
   }

data CuesheetIndex = CuesheetIndex {
    indexOffset :: Int
   ,indexNumber :: Int
   }

data APIC = Other        |
            Icon32       |
            IconOther    |
            FrontCover   |
            BackCover    |
            Leaflet      |
            CDLabel      |
            Lead         |
            Artist       |
            Conductor    |
            Band         |
            Composer     |
            Lyricist     |
            RecordingLoc |
            Recording    |
            Performance  |
            Movie        |
            Fish         |
            Illustration |
            BandLogo     |
            StudioLogo deriving (Eq, Show)

parseFLAC :: Parser [MetadataBlock]
parseFLAC = string "fLaC" >> many1 parseMetadataBlock

parseMetadataBlock :: Parser MetadataBlock
parseMetadataBlock = do
    finalBlockType <- anyWord8
    length2        <- anyWord8
    length1        <- anyWord8
    length0        <- anyWord8
    let final     = testBit finalBlockType 7
        blockTYpe = clearBit finalBlockType 7
        blocklen  = 
