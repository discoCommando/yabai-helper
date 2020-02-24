module Yabai.Frame where

import Control.Lens hiding ((.>), from, to, (|>))
import Data.Aeson
import Data.Aeson.TH
import Data.List
import Data.Ord (Ordering, compare)
import Flow
import GHC.Generics (Generic)

data Frame
  = Frame
      { _x :: Double,
        _y :: Double,
        _w :: Double,
        _h :: Double
      }
  deriving (Generic, Show)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Frame

makeLenses ''Frame

data Direction = North | West | South | East

directionCoord :: Direction -> (Frame -> Double)
directionCoord North = view y
directionCoord South = view y .> (*) (-1)
directionCoord West = view x
directionCoord East = view x .> (*) (-1)

data Range
  = Range
      {_from :: Double, _to :: Double}
  deriving (Generic, Show)

widthRange :: Frame -> Range
widthRange frame = Range (frame ^. x) (frame ^. x + frame ^. w)

heightRange :: Frame -> Range
heightRange frame = Range (frame ^. y) (frame ^. y + frame ^. h)

directionRange :: Direction -> (Frame -> Range)
directionRange North = widthRange
directionRange South = widthRange
directionRange East = heightRange
directionRange West = heightRange

makeLenses ''Range

areOverlapping :: Range -> Range -> Bool
areOverlapping range1 range2 =
  range1 ^. from <= range2 ^. to && range1 ^. to >= range2 ^. from

isRelevant :: Frame -> Direction -> Frame -> Bool
isRelevant mainFrame direction frame =
  let rangeF = directionRange direction
   in areOverlapping (rangeF mainFrame) (rangeF frame)

sortRelevantFrames :: Frame -> Direction -> [Frame] -> [Frame]
sortRelevantFrames mainFrame direction rest =
  rest
    |> filter (isRelevant mainFrame direction)
    |> sortOn (directionCoord direction)
