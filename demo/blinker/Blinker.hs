module Blinker where

import Clash.Prelude
import Clash.Annotations.TH
import Control.Monad.RWS
import Control.Lens
import qualified Veldt.Counter   as C
import qualified Veldt.PWM       as P
import qualified Veldt.Ice40.Rgb as R

type Byte = BitVector 8

data Color = Off | Red | Green | Blue | White
  deriving (NFDataX, Generic, Enum)

data Blinker = Blinker
  { _color    :: Color
  , _redPWM   :: P.PWM Byte
  , _greenPWM :: P.PWM Byte
  , _bluePWM  :: P.PWM Byte
  , _timer    :: C.Counter (Unsigned 25)
  } deriving (NFDataX, Generic)
makeLenses ''Blinker

mkBlinker :: Blinker
mkBlinker = Blinker
  { _color    = Off
  , _redPWM   = P.mkPWM 0
  , _greenPWM = P.mkPWM 0
  , _bluePWM  = P.mkPWM 0
  , _timer    = C.mkCounter 0
  }

toPWM :: Color -> (Byte, Byte, Byte)
toPWM Off   = (0,    0,    0   )
toPWM Red   = (0xFF, 0,    0   )
toPWM Green = (0,    0xFF, 0   )
toPWM Blue  = (0,    0,    0xFF)
toPWM White = (0xFF, 0xFF, 0xFF)

blinkerM :: RWS r () Blinker R.Rgb
blinkerM = do
  r <- zoom redPWM   P.pwm
  g <- zoom greenPWM P.pwm
  b <- zoom bluePWM  P.pwm
  timerDone <- zoom timer $ C.gets isTwoSeconds
  zoom timer $ C.incrementUnless isTwoSeconds
  when timerDone $ do
    c' <- color <%= nextColor
    let (redDuty', greenDuty', blueDuty') = toPWM c'
    zoom redPWM   $ P.setDuty redDuty'
    zoom greenPWM $ P.setDuty greenDuty'
    zoom bluePWM  $ P.setDuty blueDuty'
  return (r, g, b)
  where
    isTwoSeconds = (== 24000000)
    nextColor White = Off
    nextColor c     = succ c

blinker :: HiddenClockResetEnable dom => Signal dom R.Rgb
blinker = R.rgb $ mealy blinkerMealy mkBlinker $ pure ()
  where
    blinkerMealy s i = let (a, s', ()) = runRWS blinkerM i s
                       in (s', a)

{-# NOINLINE topEntity #-}
topEntity
  :: "clk" ::: Clock XilinxSystem
  -> "led" ::: Signal XilinxSystem R.Rgb
topEntity clk = withClockResetEnable clk rst enableGen blinker
  where
    rst = unsafeFromHighPolarity $ pure False
makeTopEntityWithName 'topEntity "Blinker"  

