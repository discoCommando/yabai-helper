module Yabai.Space where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Yabai.Frame (Frame)

data Space window
  = Space
      { _id :: Int,
        _windows :: [window],
        _visible :: Int
      }
  deriving (Generic, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Space

makeLenses ''Space
