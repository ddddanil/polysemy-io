{-|
Module: Polysemy.System.Args
Description: Command line arguments
Copyright: (c) Danil Doroshin, 2021
License: MIT
Maintainer: ddddanil5555@gmail.com
-}
module Polysemy.System.Args (
-- * Input
  InputArgs
, runArgsInput
-- * Reader
, ReaderArgs
, runArgsReaderToFinal
) where

import Prelude
import Polysemy
import Polysemy.Input
import Polysemy.Reader
import Polysemy.Final
import System.Environment
import Data.Maybe

-- | Type alias for 'Polysemy.Input.Input' effect.
type InputArgs = Input [String]

runArgsInput
  :: Member (Embed IO) r
  => InterpreterFor InputArgs r
runArgsInput = interpret $ \case
  Input -> embed getArgs

-- | Type alias for 'Polysemy.Reader.Reader' effect. It allows to
-- change apparent args via 'Polysemy.Reader.local' semantics.
type ReaderArgs = Reader [String]

-- | __Note__: This interpreter handles cancelled computations unsafely.
runArgsReaderToFinal
  :: Member (Final IO) r
  => InterpreterFor ReaderArgs r
runArgsReaderToFinal = interpretFinal $ \case
    Ask -> liftS getArgs
    Local f m -> do
      ins <- getInspectorS
      m' <- runS m
      liftS $ do
        args <- f <$> getArgs
        withArgs args (fromJust . inspect ins <$> m')
