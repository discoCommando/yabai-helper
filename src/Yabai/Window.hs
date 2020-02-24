module Yabai.Window where

import Control.Lens hiding ((.>), (|>))
import Data.Aeson
import Data.Aeson.TH
import Flow
import GHC.Generics
import Yabai.Frame (Frame)

data Window
  = Window
      { _id :: Int,
        _title :: String,
        _focused :: Int,
        _frame :: Frame
      }
  deriving (Generic, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Window

makeLenses ''Window

isFocused :: Window -> Bool
isFocused = view focused .> (==) 1
