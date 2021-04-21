{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Wamphf where

import Control.Lens.TH
import Control.Lens

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified ID3.Simple as ID3

data TrackMetaData = TrackMetaData {
                        _artist  :: Maybe String
                      , _title   :: Maybe String
                      , _album   :: Maybe String
                      , _year    :: Maybe String
                      , _track   :: Maybe String
                     } deriving (Generic, Show)

makeLenses ''TrackMetaData

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''TrackMetaData

fromID3Tag :: ID3.Tag -> TrackMetaData
fromID3Tag meta = TrackMetaData art titleField album year track
    where
        art = ID3.getArtist meta
        titleField = ID3.getTitle meta
        album = ID3.getAlbum meta
        year = ID3.getYear meta
        track = ID3.getTrack meta

data TrackInfo = TrackInfo {
                    _url :: FilePath
                --, duration :: Float TODO figure out how idiii or ID3v2 in general handles this
                  , _metaData :: Maybe TrackMetaData
                 } deriving (Generic, Show)

makeLenses ''TrackInfo

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''TrackInfo

readTrackInfo :: Bool -> FilePath -> IO TrackInfo
readTrackInfo pUseFname fp = do
    tag <- ID3.readTag fp
    let metadata = fromID3Tag <$> tag
    --TODO fname as title
    --TODO Path styling
    return $ TrackInfo fp metadata
