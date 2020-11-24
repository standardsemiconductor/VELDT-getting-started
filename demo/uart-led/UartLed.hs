{-# LANGUAGE LambdaCase #-}
module UartLed where

import Clash.Prelude
import Clash.Annotations.TH
import Control.Monad.RWS
import Control.Lens hiding (Index)
import Data.Maybe (fromMaybe)
import Veldt.Counter
import qualified Veldt.PWM.Rgb   as P
import qualified Veldt.Ice40.Rgb as R
import qualified Veldt.Uart      as U

type Byte = BitVector 8
type Timer = Index 36000000

data Color = Red | Green | Blue
  deriving (NFDataX, Generic)

fromColor :: Color -> (Byte, Byte, Byte)
fromColor = \case
  Red   -> (0xFF, 0x00, 0x00)
  Green -> (0x00, 0xFF, 0x00)
  Blue  -> (0x00, 0x00, 0xFF)

data Speed = Low | Mid | Hi
  deriving (NFDataX, Generic, Eq, Bounded, Enum)

toPeriod :: Speed -> Timer
toPeriod = \case
  Low -> 35999999
  Mid -> 11999999
  Hi  -> 2999999

data Instr = Speed | Color Color
  deriving (NFDataX, Generic)

encodeInstrM :: BitVector 8 -> Maybe Instr
encodeInstrM = \case
  0x73 -> Just Speed         -- 's'
  0x72 -> Just $ Color Red   -- 'r'
  0x67 -> Just $ Color Green -- 'g'
  0x62 -> Just $ Color Blue  -- 'b'
  _    -> Nothing

data Led = On | Off
  deriving (NFDataX, Generic, Eq)

toggle :: Led -> Led
toggle On  = Off
toggle Off = On

data UartLed = UartLed
  { _uart   :: U.Uart
  , _pwmRgb :: P.PWMRgb Byte
  , _speed  :: Speed
  , _led    :: Led
  , _timer  :: Timer
  } deriving (NFDataX, Generic)
makeLenses ''UartLed

mkUartLed :: UartLed
mkUartLed = UartLed
  { _uart   = U.mkUart 624
  , _pwmRgb = P.mkPWMRgb $ fromColor Red
  , _speed  = Low
  , _led    = On
  , _timer  = 0
  }

uartLed :: RWS U.Rx (First R.Rgb) UartLed ()
uartLed = do
  -- Output pwm rgb when Led on
  isOn <- uses led (== On)
  when isOn $ tell . First . Just =<< zoom pwmRgb P.pwmRgb

  -- Check toggle led
  period <- uses speed toPeriod
  t <- timer <<%= incrementUnless (== period)
  when (t == period) $ led %= toggle

  -- Update color/speed from uart
  bM <- zoom uart U.read
  forM_ (bM >>= encodeInstrM) $ \case
    Speed -> do
      speed %= increment
      timer .= 0
    Color c -> zoom pwmRgb $ P.setRgb $ fromColor c

uartLedS
  :: HiddenClockResetEnable dom
  => Signal dom Bit
  -> Signal dom R.Rgb
uartLedS = R.rgb . fmap (fromMaybe (0, 0, 0) . getFirst) . mealy uartLedMealy mkUartLed
  where
    uartLedMealy s i = let ((), s', o) = runRWS uartLed (U.Rx i) s
                       in (s', o)

{-# NOINLINE topEntity #-}
topEntity
  :: "clk" ::: Clock XilinxSystem
  -> "rx"  ::: Signal XilinxSystem Bit
  -> "led" ::: Signal XilinxSystem R.Rgb
topEntity clk = withClockResetEnable clk rst enableGen uartLedS
  where
    rst = unsafeFromHighPolarity $ pure False
makeTopEntityWithName 'topEntity "UartLed"
