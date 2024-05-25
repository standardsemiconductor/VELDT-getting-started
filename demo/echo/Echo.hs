{-# LANGUAGE LambdaCase #-}
module Echo where

import Clash.Prelude
import Clash.Annotations.TH
import Control.Monad
import Control.Monad.RWS
import Control.Lens
import qualified Veldt.Uart as U

data Fsm = Read | Write
  deriving (Generic, NFDataX)

data Echo = Echo
  { _byte :: BitVector 8
  , _uart :: U.Uart
  , _fsm  :: Fsm
  } deriving (Generic, NFDataX)
makeLenses ''Echo

mkEcho :: Echo
mkEcho = Echo
  { _byte = 0
  , _uart = U.mkUart 624
  , _fsm  = Read
  }

echoM :: RWS U.Rx U.Tx Echo ()
echoM = use fsm >>= \case
  Read -> do
    rM <- zoom uart U.read
    forM_ rM $ \r -> do
      byte .= r
      fsm .= Write
  Write -> do
    w <- use byte
    done <- zoom uart $ U.write w
    when done $ fsm .= Read

echo
  :: HiddenClockResetEnable dom
  => Signal dom Bit
  -> Signal dom Bit
echo = echoMealy <^> mkEcho
  where
    echoMealy s i = let ((), s', tx) = runRWS echoM (U.Rx i) s
                    in (s', U.unTx tx)

{-# NOINLINE topEntity #-}
topEntity
  :: "clk" ::: Clock XilinxSystem
  -> "rx"  ::: Signal XilinxSystem Bit
  -> "tx"  ::: Signal XilinxSystem Bit
topEntity clk = withClockResetEnable clk rst enableGen echo
  where
    rst = unsafeFromHighPolarity $ pure False
makeTopEntityWithName 'topEntity "Echo"
