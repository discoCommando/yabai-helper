module Yabai.Display where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics
import Yabai.Frame (Frame)

data Display space
  = Display
      { _id :: Int,
        _spaces :: [space],
        _frame :: Frame
      }
  deriving (Generic, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Display

makeLenses ''Display
