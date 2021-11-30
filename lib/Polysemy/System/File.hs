module Polysemy.System.File where

import Polysemy
import System.IO
import Prelude
import GHC.TypeLits
import Data.Singletons
import Foreign.Ptr (Ptr)
import Polysemy.System.Handle

data (File id) :: Effect where
  FileSize :: File h m Integer
  SetFileSize :: Integer -> File h m ()
  IsEOF :: File h m Bool
  SetBuffering :: BufferMode -> File h m ()
  GetBuffering :: File h m BufferMode
  Flush :: File h m ()
  -- GetPosn :: File h m HandlePosn
  -- SetPosn :: HandlePosn -> File h m ()
  Seek :: SeekMode -> Integer -> File h m ()
  Tell :: File h m Integer
  IsOpen :: File h m Bool
  IsClosed :: File h m Bool
  IsReadable :: File h m Bool
  IsWritable :: File h m Bool
  IsSeekable :: File h m Bool
  IsTerminalDevice :: File h m Bool
  SetEcho :: Bool -> File h m ()
  GetEcho :: File h m Bool
  Show :: File h m String
  WaitForInput :: Int -> File h m Bool
  IsReady :: File h m Bool
  GetChar :: File h m Char
  GetLine :: File h m String
  LookAhead :: File h m Char
  GetContents :: File h m String
  -- GetContents' :: File h m String
  PutChar :: Char -> File h m ()
  PutStr :: String -> File h m ()
  PutStrLn :: String -> File h m ()
  Print :: (Show a) => a -> File h m ()
  ReadLn :: (Read a) => File h m a
  SetBinaryMode :: Bool -> File h m ()
  PutBuf :: Ptr a -> Int -> File h m ()
  GetBuf :: Ptr a -> Int -> File h m Int
  GetBufSome :: Ptr a -> Int -> File h m Int
  PutBufNonBlocking :: Ptr a -> Int -> File h m Int
  GetBufNonBlocking :: Ptr a -> Int -> File h m Int
  SetEncoding :: TextEncoding -> File h m ()
  GetEncoding :: File h m (Maybe TextEncoding)
  SetNewlineMode :: NewlineMode -> File h m ()

makeSem ''File

interpretFile :: forall id r. Member (Embed IO) r => Handle -> InterpreterFor (File id) r
interpretFile h = interpret $ \case
  FileSize -> embed $ hFileSize h
  SetFileSize i -> embed $ hSetFileSize h i
  IsEOF -> embed $ hIsEOF h
  SetBuffering m -> embed $ hSetBuffering h m
  GetBuffering -> embed $ hGetBuffering h
  Flush -> embed $ hFlush h
  -- GetPosn -> embed $ hGetPosn h
  -- SetPosn p -> embed $ hSetPosn h p
  Seek m i -> embed $ hSeek h m i
  Tell -> embed $ hTell h
  IsOpen -> embed $ hIsOpen h
  IsClosed -> embed $ hIsClosed h
  IsReadable -> embed $ hIsReadable h
  IsWritable -> embed $ hIsWritable h
  IsReady -> embed $ hReady h
  IsSeekable -> embed $ hIsSeekable h
  IsTerminalDevice -> embed $ hIsTerminalDevice h
  SetEcho b -> embed $ hSetEcho h b
  GetEcho -> embed $ hGetEcho h
  Show -> embed $ hShow h
  WaitForInput i -> embed $ hWaitForInput h i
  GetChar -> embed $ hGetChar h
  GetLine -> embed $ hGetLine h
  LookAhead -> embed $ hLookAhead h
  GetContents -> embed $ hGetContents h
  -- GetContents' -> embed $ hGetContents' h
  PutChar c -> embed $ hPutChar h c
  PutStr s -> embed $ hPutStr h s
  PutStrLn s -> embed $ hPutStrLn h s
  Print x -> embed $ Prelude.print x
  ReadLn -> embed $ hGetLine h >>= readIO
  SetBinaryMode b -> embed $ hSetBinaryMode h b
  PutBuf p i -> embed $ hPutBuf h p i
  GetBuf p i -> embed $ hGetBuf h p i
  GetBufSome p i -> embed $ hGetBufSome h p i
  PutBufNonBlocking p i -> embed $ hPutBufNonBlocking h p i
  GetBufNonBlocking p i -> embed $ hGetBufNonBlocking h p i
  SetEncoding e -> embed $ hSetEncoding h e
  GetEncoding -> embed $ hGetEncoding h
  SetNewlineMode m -> embed $ hSetNewlineMode h m

interpretFileH :: forall h r. (KnownHandle h, Member (Embed IO) r) => InterpreterFor (File (h :: Handle)) r
interpretFileH = let handle = handleVal (Proxy :: Proxy h)
   in interpretFile handle

interpretFileS :: forall s r. (KnownSymbol s, Member (Embed IO) r) => IOMode -> InterpreterFor (File (s :: Symbol)) r
interpretFileS mode m0 = do
  let path = symbolVal (Proxy :: Proxy s)
  handle <- embed $ openFile path mode
  interpretFile handle m0

