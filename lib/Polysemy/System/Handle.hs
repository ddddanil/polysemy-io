module Polysemy.System.Handle (
  KnownHandle, handleVal
, SomeHandle(..), someHandleVal
) where

import System.IO
-- import Data.Kind (Type)
-- import Prelude
import Data.Singletons
import GHC.Exts

newtype SHandle (h :: Handle) = SHandle Handle
type SStdin = SHandle stdin

class KnownHandle (h :: Handle) where
  handleSing :: SHandle h

handleVal :: forall h proxy. KnownHandle h => proxy h -> Handle
handleVal _ = case handleSing :: SHandle h of
                 SHandle h -> h

data SomeHandle = forall h. KnownHandle h => SomeHandle (Proxy h)

someHandleVal :: Handle -> SomeHandle
someHandleVal h = withSHandle SomeHandle (SHandle h) Proxy

data WrapH h a = WrapH (KnownHandle h => Proxy h -> a)

withSHandle :: (KnownHandle h => Proxy h -> a)
            -> SHandle h      -> Proxy h -> a
withSHandle f x y = magicDict (WrapH f) x y

{-
type SHandle' :: Handle -> Type
data SHandle' (h :: Handle) = KnownHandle h => SHandle'
type instance Sing = SHandle'

instance KnownHandle h => SingI (h :: Handle) where
  sing = SHandle'

instance SingKind Handle where
  type Demote Handle = Handle

  fromSing :: Sing (h :: Handle) -> Handle
  fromSing (SHandle' :: Sing h) = handleVal (Proxy :: Proxy h)

  toSing :: Handle -> SomeSing Handle
  toSing h = case someHandleVal h of
    SomeHandle (_ :: Proxy h) -> SomeSing (SHandle' :: Sing h)
-}
