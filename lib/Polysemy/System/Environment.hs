module Polysemy.System.Environment (
  Environment
, getVar, setVar, unsetVar
, runEnvironment
, runEnvInput, runEnvCached
) where

import Prelude
import Polysemy
import Polysemy.Input
import Polysemy.View
import System.Environment
import Data.Maybe

data Environment :: Effect where
  GetVar :: String -> Environment m (Maybe String)
  SetVar :: String -> Maybe String -> Environment m ()

makeSem ''Environment

unsetVar :: Member Environment r => String -> Sem r ()
unsetVar var = setVar var Nothing

runEnvironment
  :: Member (Embed IO) r
  => InterpreterFor Environment r
runEnvironment = interpret $ \case
  GetVar var -> embed $ lookupEnv var
  SetVar var val -> embed . setEnv var . fromMaybe "" $ val

type InputEnv = Input [(String, String)]

runEnvInput
  :: Member (Embed IO) r
  => InterpreterFor InputEnv r
runEnvInput = runInputSem $ embed getEnvironment
