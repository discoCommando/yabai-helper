{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Data.Semigroup
import Options.Applicative
import Polysemy
import Polysemy.Error
import Shell
import Yabai
import Yabai.Frame (Direction (..))

data Config = Config {_direction :: Direction}

makeLenses ''Config

direction' :: Parser Direction
direction' =
  flag'
    North
    ( long "north"
        <> short 'n'
    )
    <|> flag'
      South
      ( long "south"
          <> short 's'
      )
    <|> flag'
      East
      ( long "east"
          <> short 'e'
      )
    <|> flag'
      West
      ( long "west"
          <> short 'w'
      )

config :: Parser Config
config = Config <$> direction'

main :: IO ()
main =
  execParser opts
    >>= ( \config -> do
            result <-
              runM
                . runError @String
                . shellToIO
                . yabaiFromShell
                $ focus
                $ config ^. direction
            case result of
              Right () -> return ()
              Left err -> putStrLn err
        )
  where
    opts =
      info
        (config <**> helper)
        ( fullDesc
            <> progDesc "Move focus to window in a given direction"
        )
