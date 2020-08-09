module Veldt.Counter
  ( Counter
  , mkCounter
  , increment
  , incrementWhen
  , incrementUnless
  , decrement
  , set
  , get
  , gets
  ) where

import Clash.Prelude
import Control.Monad.RWS (RWST)
import qualified Control.Monad.RWS as RWS

-------------
-- Counter --
-------------
newtype Counter a = Counter { unCounter :: a }
  deriving (NFDataX, Generic)

mkCounter :: a -> Counter a
mkCounter = Counter

set :: (Monoid w, Monad m) => a -> RWST r w (Counter a) m ()
set = RWS.put . Counter

get :: (Monoid w, Monad m) => RWST r w (Counter a) m a
get = RWS.gets unCounter

gets :: (Monoid w, Monad m) => (a -> b) -> RWST r w (Counter a) m b
gets f = f <$> get

increment :: (Monoid w, Monad m, Bounded a, Enum a, Eq a) => RWST r w (Counter a) m ()
increment = do
  c <- get
  set $ if c == maxBound
    then minBound
    else succ c

decrement :: (Monoid w, Monad m, Bounded a, Enum a, Eq a) => RWST r w (Counter a) m ()
decrement = do
  c <- get
  set $ if c == minBound
    then maxBound
    else pred c

incrementWhen
  :: (Monoid w, Monad m, Bounded a, Enum a, Eq a)
  => (a -> Bool)
  -> RWST r w (Counter a) m ()
incrementWhen p = do
  b <- gets p
  if b
    then increment
    else set minBound

incrementUnless
  :: (Monoid w, Monad m, Bounded a, Enum a, Eq a)
  => (a -> Bool)
  -> RWST r w (Counter a) m ()
incrementUnless p = incrementWhen (not . p)
