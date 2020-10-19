module Veldt.PWM
  ( PWM
  , mkPWM
  , pwm
  , setDuty
  ) where

import Clash.Prelude
import Control.Lens
import Control.Monad.RWS
import qualified Veldt.Counter as C

---------
-- PWM --
---------
data PWM a = PWM
  { _ctr  :: C.Counter a
  , _duty :: a
  } deriving (NFDataX, Generic)
makeLenses ''PWM

mkPWM :: Bounded a => a -> PWM a
mkPWM = PWM (C.mkCounter minBound)

setDuty :: (Monoid w, Monad m, Bounded a) => a -> RWST r w (PWM a) m ()
setDuty d = do
  duty .= d
  zoom ctr $ C.set minBound

pwm :: (Monoid w, Monad m, Ord a, Bounded a, Enum a) => RWST r w (PWM a) m Bit
pwm = do
  d <- use duty
  c <- zoom ctr C.get
  zoom ctr C.increment
  return $ boolToBit $ c < d

  
