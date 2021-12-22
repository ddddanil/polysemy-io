module Polysemy.System.Timeout (
  Timeout, timeout
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

