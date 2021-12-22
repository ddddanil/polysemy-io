{-|
Module: Polysemy.System.Environment
Description: Environment variables
Copyright: (c) Danil Doroshin, 2021
License: MIT
Maintainer: ddddanil5555@gmail.com
-}
module Polysemy.System.Environment (
-- * Effect
  Environment
-- ** Actions
, getVar, setVar, unsetVar
-- ** Interpreters
, runEnvironment
-- * Input
, InputEnv
-- ** Interpreters
, runEnvInput
) where

import Prelude
import Polysemy
import Polysemy.Input
import System.Environment
import Data.Maybe

-- | Gives read and write access to the environment variables.
--
-- Note: for the environment, unsetting a variable is equivalent to setting it to ""
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

-- | Type alias for the 'Polysemy.Input.Input' effect. 'IO' operations
-- are performed for every query
type InputEnv = Input [(String, String)]

runEnvInput
  :: Member (Embed IO) r
  => InterpreterFor InputEnv r
runEnvInput = runInputSem $ embed getEnvironment
