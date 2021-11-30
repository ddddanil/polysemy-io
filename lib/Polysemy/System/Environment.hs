module Polysemy.System.Environment (
  Environment
, getVar, setVar, unsetVar
, runEnvironment
) where

import Prelude
import Polysemy
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

