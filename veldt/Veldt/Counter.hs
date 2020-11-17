module Veldt.Counter
  ( increment
  , incrementWhen
  , incrementUnless
  , decrement
  ) where

import Clash.Prelude

-------------
-- Counter --
-------------
increment :: (Bounded a, Enum a, Eq a) => a -> a
increment a
  | a == maxBound = minBound
  | otherwise = succ a          

decrement :: (Bounded a, Enum a, Eq a) => a -> a
decrement a
  | a == minBound = maxBound
  | otherwise = pred a

incrementWhen :: (Bounded a, Enum a, Eq a) => (a -> Bool) -> a -> a
incrementWhen p a
  | p a = increment a
  | otherwise = minBound

incrementUnless :: (Bounded a, Enum a, Eq a) => (a -> Bool) -> a -> a
incrementUnless p = incrementWhen (not . p)
