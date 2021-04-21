{-# LANGUAGE DeriveGeneric #-}

module Wamphf (
    TrackMetaData,
    TrackInfo,
    readTrackInfo
  ) where

import GHC.Generics
import Data.Aeson
import qualified ID3.Simple as ID3

data TrackMetaData = TrackMetaData {
                        artist  :: Maybe String
                      , title   :: Maybe String
                      , album   :: Maybe String
                      , year    :: Maybe String
                      , track   :: Maybe String
                     } deriving (Generic, Show)

instance ToJSON TrackMetaData where
    toEncoding = genericToEncoding defaultOptions

fromID3Tag :: ID3.Tag -> TrackMetaData
fromID3Tag meta = TrackMetaData art titleField album year track
    where
        art = ID3.getArtist meta
        titleField = ID3.getTitle meta
        album = ID3.getAlbum meta
        year = ID3.getYear meta
        track = ID3.getTrack meta

data TrackInfo = TrackInfo {
                    url :: FilePath
                --, duration :: Float TODO figure out how idiii or ID3v2 in general handles this
                  , metaData :: Maybe TrackMetaData
                 } deriving (Generic, Show)

instance ToJSON TrackInfo where
    toEncoding = genericToEncoding defaultOptions

readTrackInfo :: Bool -> FilePath -> IO TrackInfo
readTrackInfo pUseFname fp = do
    tag <- ID3.readTag fp
    let metadata = fromID3Tag <$> tag
    --TODO fname as title
    --TODO Path styling
    return $ TrackInfo fp metadata
