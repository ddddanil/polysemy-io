{-|
Module: Polysemy.IO.File
Description: File operations
Copyright: (c) Danil Doroshin, 2021
License: MIT
Maintainer: ddddanil5555@gmail.com
-}
module Polysemy.IO.File (
-- * Effect
  File
-- ** Actions
, flush, show
-- *** Writing
, putChar, putStr, putStrLn, print
-- *** Reading
, getChar, getLine, lookAhead, getContents
, readLn, isReady, waitForInput
-- *** Seeking
, F.SeekMode(..)
, seek, tell
-- *** File size
, fileSize, setFileSize
-- *** State
, isEOF, isOpen, isClosed, isReadable, isWritable
, isSeekable, isTerminalDevice, isReady
-- *** Buffering
, F.BufferMode(..)
, setBuffering, getBuffering
-- *** Encoding
, F.TextEncoding
, getEncoding, setEncoding
-- *** Echo
, getEcho, setEcho
-- *** Modes
, setBinaryMode
, F.NewlineMode(..)
, setNewlineMode
-- *** Buffer IO
, putBuf, getBuf, getBufSome
, putBufNonBlocking, getBufNonBlocking
-- ** Interpreters
, runFile
, runFileH
, runFileS
, runFileWithPath
) where

import Polysemy
import Polysemy.Tagged
import qualified System.IO as F
import Prelude hiding
  ( putChar, putStr, putStrLn, print
  , getChar, getLine, getContents, readLn
  , show)
import GHC.TypeLits
import Data.Proxy
import Foreign.Ptr (Ptr)
import Polysemy.IO.File.Handle

data File :: Effect where
  FileSize :: File m Integer
  SetFileSize :: Integer -> File m ()
  IsEOF :: File m Bool
  SetBuffering :: F.BufferMode -> File m ()
  GetBuffering :: File m F.BufferMode
  Flush :: File m ()
  -- GetPosn :: File m HandlePosn
  -- SetPosn :: HandlePosn -> File m ()
  Seek :: F.SeekMode -> Integer -> File m ()
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
  SetEncoding :: F.TextEncoding -> File m ()
  GetEncoding :: File m (Maybe F.TextEncoding)
  SetNewlineMode :: F.NewlineMode -> File m ()

makeSem ''File

runFile
  :: Member (Embed IO) r
  => F.Handle
  -> InterpreterFor File r
runFile h = interpret $ \case
  FileSize -> embed $ F.hFileSize h
  SetFileSize i -> embed $ F.hSetFileSize h i
  IsEOF -> embed $ F.hIsEOF h
  SetBuffering m -> embed $ F.hSetBuffering h m
  GetBuffering -> embed $ F.hGetBuffering h
  Flush -> embed $ F.hFlush h
  -- GetPosn -> embed $ F.hGetPosn h
  -- SetPosn p -> embed $ F.hSetPosn h p
  Seek m i -> embed $ F.hSeek h m i
  Tell -> embed $ F.hTell h
  IsOpen -> embed $ F.hIsOpen h
  IsClosed -> embed $ F.hIsClosed h
  IsReadable -> embed $ F.hIsReadable h
  IsWritable -> embed $ F.hIsWritable h
  IsReady -> embed $ F.hReady h
  IsSeekable -> embed $ F.hIsSeekable h
  IsTerminalDevice -> embed $ F.hIsTerminalDevice h
  SetEcho b -> embed $ F.hSetEcho h b
  GetEcho -> embed $ F.hGetEcho h
  Show -> embed $ F.hShow h
  WaitForInput i -> embed $ F.hWaitForInput h i
  GetChar -> embed $ F.hGetChar h
  GetLine -> embed $ F.hGetLine h
  LookAhead -> embed $ F.hLookAhead h
  GetContents -> embed $ F.hGetContents h
  -- GetContents' -> embed $ F.hGetContents' h
  PutChar c -> embed $ F.hPutChar h c
  PutStr s -> embed $ F.hPutStr h s
  PutStrLn s -> embed $ F.hPutStrLn h s
  Print x -> embed $ F.print x
  ReadLn -> embed $ F.hGetLine h >>= readIO
  SetBinaryMode b -> embed $ F.hSetBinaryMode h b
  PutBuf p i -> embed $ F.hPutBuf h p i
  GetBuf p i -> embed $ F.hGetBuf h p i
  GetBufSome p i -> embed $ F.hGetBufSome h p i
  PutBufNonBlocking p i -> embed $ F.hPutBufNonBlocking h p i
  GetBufNonBlocking p i -> embed $ F.hGetBufNonBlocking h p i
  SetEncoding e -> embed $ F.hSetEncoding h e
  GetEncoding -> embed $ F.hGetEncoding h
  SetNewlineMode m -> embed $ F.hSetNewlineMode h m

runFileH
  :: forall h r. (KnownHandle h, Member (Embed IO) r)
  => InterpreterFor (Tagged (h :: F.Handle) File) r
runFileH = let
  handle = handleVal (Proxy :: Proxy h)
  in runFile handle . untag

runFileS
  :: forall s r. (KnownSymbol s, Member (Embed IO) r)
  => F.IOMode
  -> InterpreterFor (Tagged (s :: Symbol) File) r
runFileS mode = let
  path = symbolVal (Proxy :: Proxy s)
  in runFileWithPath path mode . untag

runFileWithPath
  :: Member (Embed IO) r
  => FilePath
  -> F.IOMode
  -> InterpreterFor File r
runFileWithPath path mode m0 = do
  handle <- embed $ F.openFile path mode
  runFile handle m0

