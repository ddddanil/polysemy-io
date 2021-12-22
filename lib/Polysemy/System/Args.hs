module Polysemy.System.Args (
  Args, ArgsReader
, runArgsInput
, runArgsReaderToFinal
) where

import Prelude
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Polysemy.Final
import System.Environment
import Data.Maybe

type Args = [String]
type ArgsReader = Reader Args
type ArgsInput = Input Args

runArgsInput
  :: Member (Embed IO) r
  => InterpreterFor ArgsInput r
runArgsInput = interpret $ \case
  Input -> embed getArgs

runArgsReaderToFinal
  :: Member (Final IO) r
  => InterpreterFor ArgsReader r
runArgsReaderToFinal = interpretFinal $ \case
    Ask -> liftS getArgs
    Local f m -> do
      ins <- getInspectorS
      m' <- runS m
      liftS $ f <$> getArgs >>= \args -> withArgs args (fromJust . inspect ins <$> m')
