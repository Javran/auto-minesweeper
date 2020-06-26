{-# LANGUAGE OverloadedStrings #-}

module Info where

import Control.Monad
import Data.Aeson
import qualified Data.Vector as V

data Info = Info
  { windowId :: Int
  , topLeft :: (Int, Int)
  , tilesShape :: (Int, Int)
  , tileSize :: (Int, Int)
  , tileShrink :: Int
  }
  deriving (Show)

instance FromJSON Info where
  parseJSON = withObject "Info" $ \o -> do
    let parsePair tag = withArray tag $ \xs -> do
          guard $ V.length xs == 2
          (,)
            <$> parseJSON (xs V.! 0)
            <*> parseJSON (xs V.! 1)
    Info
      <$> o .: "window_id"
      <*> (parsePair "TopLeft" =<< o .: "top_left")
      <*> (parsePair "TilesShape" =<< o .: "tiles_shape")
      <*> (parsePair "TileSize" =<< o .: "tile_size")
      <*> o .: "tile_shrink"
