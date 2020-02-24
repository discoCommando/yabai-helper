{-# LANGUAGE TypeApplications #-}

module Yabai where

import Control.Lens hiding ((|>))
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Data.List
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId)
import Flow ((|>))
import Polysemy
import Polysemy.Error
import Shell
import Yabai.Frame
import Yabai.Space
import Yabai.Window

data Yabai m a where
  GetWindowsFromCurrentSpace :: Yabai m [Window]
  GetWindowsFromVisibleSpaces :: Yabai m [Space Window]
  SetFocusedWindow :: Int -> Yabai m ()

makeSem ''Yabai

type ErrorEff = Polysemy.Error.Error String

pureYabai :: [Space Window] -> Sem (Yabai ': r) a -> Sem r a
pureYabai spaces =
  interpret $ \case
    GetWindowsFromCurrentSpace ->
      return $
        spaces
          ^.. folded . windows . folded
    GetWindowsFromVisibleSpaces -> return spaces
    SetFocusedWindow i -> return ()

yabaiFromShell :: Members '[Shell, ErrorEff] r => Sem (Yabai ': r) a -> Sem r a
yabaiFromShell = interpret $ \case
  GetWindowsFromCurrentSpace -> do
    stringResult <-
      Shell.runCommand "yabai" ["-m", "query", "--windows", "--space"] ""
        |> fmap (pack)
    decode @[Window] stringResult
      |> maybe (throw "Decoding error for querying windows in the focused space") return
  GetWindowsFromVisibleSpaces ->
    do
      stringSpaces <-
        Shell.runCommand "yabai" ["-m", "query", "--spaces"] ""
          |> fmap pack
      spaceInts <-
        decode @[Space Int] stringSpaces
          |> maybe (throw "Decoding error for querying spaces in displays") return
          |> fmap (filter (\x -> view visible x == 1))
      stringWindows <-
        Shell.runCommand "yabai" ["-m", "query", "--windows"] ""
          |> fmap pack
      windows' <-
        decode @[Window] stringWindows
          |> maybe (throw "Decoding error for querying all windows") return
      spaceInts
        |> traversed
          . windows
          . traversed
          %~ (\i -> find (\w -> w ^. Yabai.Window.id == i) windows')
        |> sequenceAOf (traversed . windows . traversed)
        |> maybe (throw "One of the windows was not found") return
  SetFocusedWindow id' ->
    Shell.runCommand "yabai" ["-m", "window", "--focus", show id'] ""
      >> return ()

focus :: Members '[Yabai, Error String] r => Yabai.Frame.Direction -> Sem r ()
focus direction = do
  spaces <- getWindowsFromVisibleSpaces
  let windows' = spaces ^.. traversed . windows . traversed
  let focusedWindow = windows' |> Data.List.find Yabai.Window.isFocused
  case focusedWindow of
    Nothing -> throw "No focused window found"
    Just focusedWindow ->
      do
        let notFocusedWindows = windows' |> Data.List.filter (not .> Yabai.Window.isFocused)
        let relevantWindows =
              notFocusedWindows
                |> filter
                  ( \w ->
                      w ^. Yabai.Window.frame
                        |> Yabai.Frame.isRelevant (focusedWindow ^. Yabai.Window.frame) direction
                  )
        let sortedRelevantWindows =
              ( focusedWindow
                  : relevantWindows
              )
                |> sortOn (\w -> view Yabai.Window.frame w |> Yabai.Frame.directionCoord direction)
        let windowBefore =
              sortedRelevantWindows
                ^.. takingWhile (not .> Yabai.Window.isFocused) folded
                ^.. _last
        case windowBefore of
          [] -> return ()
          (w : _) ->
            setFocusedWindow (w ^. Yabai.Window.id)
