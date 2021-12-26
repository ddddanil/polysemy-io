module Polysemy.IO.Network.Socket (
-- * Effect
  Connection
-- * Actions
, getSocket
, connect, bind, listen, accept
-- * Interpreters
, runWithSocket
) where

import Prelude
import Polysemy
import Polysemy.Bundle
import Polysemy.Input
import qualified Network.Socket as S

type SocketInner = '[Input S.Socket, Embed IO]
type SocketBundle = Bundle SocketInner
newtype Connection m a = Socket { unSocket :: SocketBundle m a }

liftE :: forall e r a. (Member e SocketInner, Member Connection r) => Sem (e ': r) a -> Sem r a
liftE = transform Socket . rewrite injBundle

getSocket :: Member Connection r => Sem r S.Socket
getSocket = liftE input

liftIO :: Member Connection r => IO a -> Sem r a
liftIO = liftE @(Embed IO) . embed

connect :: Member Connection r => S.SockAddr -> Sem r ()
connect addr = do
  s <- getSocket
  liftIO $ S.connect s addr

bind :: Member Connection r => S.SockAddr -> Sem r ()
bind addr = do
  s <- getSocket
  liftIO $ S.bind s addr

listen :: Member Connection r => Int -> Sem r ()
listen i = do
  s <- getSocket
  liftIO $ S.listen s i

accept :: Member Connection r => Sem r (S.Socket, S.SockAddr)
accept = do
  s <- getSocket
  liftIO $ S.accept s

runWithSocket :: Member (Embed IO) r => S.Socket -> InterpreterFor Connection r
runWithSocket sock = subsume . runInputConst sock . runBundle . rewrite unSocket

