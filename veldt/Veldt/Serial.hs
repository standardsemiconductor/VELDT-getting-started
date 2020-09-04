module Veldt.Serial
  ( Direction(..)
  -- Deserializer
  , Deserializer
  , mkDeserializer
  , full
  , deserialize
  , get
  , clear
  -- Serializer
  , Serializer
  , mkSerializer
  , empty
  , serialize
  , peek
  , give
  ) where

import Clash.Prelude hiding (empty)
import Control.Monad.RWS (RWST)
import Control.Lens hiding (Index)
import qualified Veldt.Counter as C

data Direction = L | R
  deriving (NFDataX, Generic)

------------------                                                                            
-- Deserializer --                                                                            
------------------     
data Deserializer n a = Deserializer
  { _dBuf  :: Vec n a
  , _dFull :: Bool
  , _dCtr  :: C.Counter (Index n)
  , _dDir  :: Direction
  } deriving (NFDataX, Generic)
makeLenses ''Deserializer

mkDeserializer :: KnownNat n => a -> Direction -> Deserializer n a
mkDeserializer a = Deserializer (repeat a) False (C.mkCounter 0)

full :: (Monoid w, Monad m) => RWST r w (Deserializer n a) m Bool
full = use dFull

deserialize :: (Monoid w, Monad m, KnownNat n) => a -> RWST r w (Deserializer n a) m ()
deserialize d = do
  use dDir >>= \case
    R -> dBuf %= (<<+ d)
    L -> dBuf %= (d +>>)
  dFull <~ zoom dCtr (C.gets (== maxBound))
  zoom dCtr C.increment
    
get :: (Monoid w, Monad m) => RWST r w (Deserializer n a) m (Vec n a)
get = use dBuf

clear :: (Monoid w, Monad m, KnownNat n) => RWST r w (Deserializer n a) m ()
clear = do
  dFull .= False
  zoom dCtr $ C.set 0

----------------
-- Serializer --
----------------
data Serializer n a = Serializer
  { _sBuf   :: Vec n a
  , _sEmpty :: Bool
  , _sCtr   :: C.Counter (Index n)
  , _sDir   :: Direction
  } deriving (NFDataX, Generic)
makeLenses ''Serializer

mkSerializer :: KnownNat n => a -> Direction -> Serializer n a
mkSerializer a = Serializer (repeat a) False (C.mkCounter 0) 

serialize :: (Monoid w, Monad m, KnownNat n) => RWST r w (Serializer n a) m ()
serialize = do
  use sDir >>= \case
    R -> sBuf %= (`rotateRightS` d1)
    L -> sBuf %= (`rotateLeftS`  d1)
  sEmpty <~ zoom sCtr (C.gets (== maxBound))
  zoom sCtr C.increment

peek :: (Monoid w, Monad m, KnownNat n) => RWST r w (Serializer (n + 1) a) m a
peek = use sDir >>= \case
  R -> uses sBuf last
  L -> uses sBuf head

give :: (Monoid w, Monad m, KnownNat n) => Vec n a -> RWST r w (Serializer n a) m ()
give v = do
  sBuf .= v
  sEmpty .= False
  zoom sCtr $ C.set 0

empty :: (Monoid w, Monad m) => RWST r w (Serializer n a) m Bool
empty = use sEmpty
