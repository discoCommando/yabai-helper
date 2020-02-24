module Shell where

import Polysemy
import System.Process

data Shell m a where
  RunCommand :: String -> [String] -> String -> Shell m String

makeSem ''Shell

shellToIO :: Member (Embed IO) r => Sem (Shell ': r) a -> Sem r a
shellToIO = interpret $ \case
  RunCommand cmd args input -> embed $ readProcess cmd args input
