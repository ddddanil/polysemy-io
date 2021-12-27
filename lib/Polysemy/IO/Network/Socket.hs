module Polysemy.IO.Network.Socket (
-- * Effect
  Connection
-- * Actions
, getSocket
, connect, bind, listen, accept
-- * Adapters
, recvInput, sendOutput, sendOutputBatch, recvBuf
-- * Interpreters
, runWithSocket
, withSockets
) where

import Prelude
import Polysemy
import Polysemy.Final
import Polysemy.Bundle
import Polysemy.Input
import Polysemy.Output
import Data.ByteString
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import Foreign (Ptr, Word8)
import Foreign.Marshal.Array (mallocArray)

type SocketInner = '[Input S.Socket, Embed IO]
newtype Connection m a = Socket { unSocket :: Bundle SocketInner m a }

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

recvInput :: Member Connection r => Int -> InterpreterFor (Input ByteString) r
recvInput size m = do
  s <- getSocket
  let recv = liftIO $ SB.recv s size
  runInputSem recv m

sendOutput :: Member Connection r => InterpreterFor (Output ByteString) r
sendOutput m = do
  s <- getSocket
  let send msg = liftIO $ SB.sendAll s msg
  runOutputSem send m

sendOutputBatch :: Member Connection r => InterpreterFor (Output ByteString) r
sendOutputBatch m = do
  s <- getSocket
  (msgs, res) <- runOutputList m
  liftIO $ SB.sendMany s msgs
  return res

recvBuf :: Member Connection r => Int -> Sem (Input (Ptr Word8, Int) ': r) a -> Sem r a
recvBuf size m = do
  a <- liftIO $ mallocArray size
  s <- getSocket
  let recv = liftIO $ (a, ) <$> S.recvBuf s a size
  runInputSem recv m

runWithSocket :: Member (Embed IO) r => S.Socket -> InterpreterFor Connection r
runWithSocket sock = subsume . runInputConst sock . runBundle . rewrite unSocket

withSockets :: Member (Final IO) r => Sem r a -> Sem r (Maybe a)
withSockets m = withStrategicToFinal $ do
  ins <- getInspectorS
  m' <- runS m
  liftS $ S.withSocketsDo (inspect ins <$> m')

