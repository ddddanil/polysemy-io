{-|
Module: Polysemy.System.Timeout
Description: Timeout
Copyright: (c) Danil Doroshin, 2021
License: MIT
Maintainer: ddddanil5555@gmail.com
-}
module Polysemy.System.Timeout (
-- * Effect
  Timeout
-- ** Actions
, timeout
-- ** Interpreters
, runTimeoutToFinalIO
) where

import Prelude
import Polysemy
import qualified System.Timeout as T
import Polysemy.Final
import Control.Monad (join)

data Timeout :: Effect where
  Timeout :: Int -> m a -> Timeout m (Maybe a)

makeSem ''Timeout

runTimeoutToFinalIO
  :: Member (Final IO) r
  => InterpreterFor Timeout r
runTimeoutToFinalIO = interpretFinal $ \case
  Timeout micros m -> do
    ins <- getInspectorS
    m' <- runS m
    liftS $ join <$> T.timeout micros (inspect ins <$> m')

