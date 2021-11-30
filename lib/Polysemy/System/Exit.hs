module Polysemy.System.Exit (
  Exit
, exitWith, exitSuccess, exitFailure, die
, runExit
, SE.ExitCode(..)
) where

import Prelude
import Polysemy
import qualified System.Exit as SE

data Exit :: Effect where
  ExitWith :: SE.ExitCode -> Exit m a
  ExitSuccess :: Exit m a
  ExitFailure :: Exit m a
  Die :: String -> Exit m a

makeSem ''Exit

runExit
  :: Member (Embed IO) r
  => InterpreterFor Exit r
runExit = interpret $ \case
  ExitWith c -> embed $ SE.exitWith c
  ExitSuccess -> embed SE.exitSuccess 
  ExitFailure -> embed SE.exitFailure 
  Die s -> embed $ SE.die s

