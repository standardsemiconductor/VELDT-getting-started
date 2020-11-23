module Veldt.PWM.Rgb where

import Clash.Prelude
import Control.Lens
import Control.Monad.RWS
import Veldt.PWM
import Veldt.Ice40.Rgb (Rgb)

-----------
-- Types --
-----------
data PWMRgb a = PWMRgb
  { _redPWM   :: PWM a
  , _greenPWM :: PWM a
  , _bluePWM  :: PWM a
  } deriving (NFDataX, Generic)
makeLenses ''PWMRgb

mkPWMRgb :: Bounded a => (a, a, a) -> PWMRgb a
mkPWMRgb (r, g, b) = PWMRgb
  { _redPWM   = mkPWM r
  , _greenPWM = mkPWM g
  , _bluePWM  = mkPWM b
  }

---------
-- API --
---------
pwmRgb :: (Monoid w, Monad m, Ord a, Bounded a, Enum a) => RWST r w (PWMRgb a) m Rgb
pwmRgb = do
  r <- zoom redPWM pwm
  g <- zoom greenPWM pwm
  b <- zoom bluePWM pwm
  return (r, g, b)

setRgb :: (Monoid w, Monad m, Bounded a) => (a, a, a) -> RWST r w (PWMRgb a) m ()
setRgb (r, g, b) = do
  zoom redPWM   $ setDuty r
  zoom greenPWM $ setDuty g
  zoom bluePWM  $ setDuty b
