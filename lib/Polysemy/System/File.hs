module Polysemy.System.File where

import Polysemy
import Polysemy.Tagged
import System.IO
import Prelude
import GHC.TypeLits
import Data.Proxy
import Foreign.Ptr (Ptr)
import Polysemy.System.Handle

data File :: Effect where
  FileSize :: File m Integer
  SetFileSize :: Integer -> File m ()
  IsEOF :: File m Bool
  SetBuffering :: BufferMode -> File m ()
  GetBuffering :: File m BufferMode
  Flush :: File m ()
  -- GetPosn :: File m HandlePosn
  -- SetPosn :: HandlePosn -> File m ()
  Seek :: SeekMode -> Integer -> File m ()
  Tell :: File m Integer
  IsOpen :: File m Bool
  IsClosed :: File m Bool
  IsReadable :: File m Bool
  IsWritable :: File m Bool
  IsSeekable :: File m Bool
  IsTerminalDevice :: File m Bool
  SetEcho :: Bool -> File m ()
  GetEcho :: File m Bool
  Show :: File m String
  WaitForInput :: Int -> File m Bool
  IsReady :: File m Bool
  GetChar :: File m Char
  GetLine :: File m String
  LookAhead :: File m Char
  GetContents :: File m String
  -- GetContents' :: File m String
  PutChar :: Char -> File m ()
  PutStr :: String -> File m ()
  PutStrLn :: String -> File m ()
  Print :: (Show a) => a -> File m ()
  ReadLn :: (Read a) => File m a
  SetBinaryMode :: Bool -> File m ()
  PutBuf :: Ptr a -> Int -> File m ()
  GetBuf :: Ptr a -> Int -> File m Int
  GetBufSome :: Ptr a -> Int -> File m Int
  PutBufNonBlocking :: Ptr a -> Int -> File m Int
  GetBufNonBlocking :: Ptr a -> Int -> File m Int
  SetEncoding :: TextEncoding -> File m ()
  GetEncoding :: File m (Maybe TextEncoding)
  SetNewlineMode :: NewlineMode -> File m ()

makeSem ''File

runFile
  :: Member (Embed IO) r
  => Handle
  -> InterpreterFor File r
runFile h = interpret $ \case
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

runFileH
  :: forall h r. (KnownHandle h, Member (Embed IO) r)
  => InterpreterFor (Tagged (h :: Handle) File) r
runFileH = let
  handle = handleVal (Proxy :: Proxy h)
  in runFile handle . untag

runFileS
  :: forall s r. (KnownSymbol s, Member (Embed IO) r)
  => IOMode
  -> InterpreterFor (Tagged (s :: Symbol) File) r
runFileS mode = let
  path = symbolVal (Proxy :: Proxy s)
  in runFileWithPath path mode . untag

runFileWithPath
  :: Member (Embed IO) r
  => FilePath
  -> IOMode
  -> InterpreterFor File r
runFileWithPath path mode m0 = do
  handle <- embed $ openFile path mode
  runFile handle m0

