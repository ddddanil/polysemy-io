{-|
Module: Polysemy.System.Exit
Description: Forced exit
Copyright: (c) Danil Doroshin, 2021
License: MIT
Maintainer: ddddanil5555@gmail.com
-}
module Polysemy.System.Exit (
-- * Effect
  Exit
, SE.ExitCode(..)
-- ** Actions
, exitWith, exitSuccess, exitFailure, die
-- ** Interpreters
, runExit
-- * Fail
, failToIO
) where

import Prelude
import Polysemy
import Polysemy.Fail
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

failToIO
  :: Member (Embed IO) r
  => InterpreterFor Fail r
failToIO = interpret $ \case
  Fail s -> embed $ SE.die s

