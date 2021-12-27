{-|
Module: Polysemy.IO.Open
Description: Effect for resource allocation
Copyright: (c) Danil Doroshin, 2021
License: MIT
Maintainer: ddddanil5555@gmail.com
-}
module Polysemy.IO.Open (
-- * Effect
  Open(..)
, OpenArgs
-- ** Actions
, open
) where

import Polysemy

-- | Where you should define the most generic set of arguments to open a resource
type family OpenArgs o

-- | A generic effect that signifies the capability to open some resource
-- (ex. 'Network.Socket.Socket', 'GHC.IO.Handle.Handle')
data (Open o) :: Effect where
  Open :: OpenArgs o -> Open o m o

makeSem ''Open

