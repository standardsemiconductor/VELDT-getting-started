module Veldt.Uart
  ( Rx(Rx)
  , unRx
  , Tx(Tx)
  , unTx
  , Byte
  , Uart
  , mkUart
  , read
  , write
  ) where

import Clash.Prelude hiding (read)
import Control.Monad.RWS
import Control.Lens hiding ((:>))
import qualified Veldt.Counter as C
import qualified Veldt.Serial  as S

type Byte = BitVector 8

newtype Rx = Rx { unRx :: Bit }
newtype Tx = Tx { unTx :: Bit }

instance Semigroup Tx where
  Tx tx <> Tx tx' = Tx $ tx .&. tx'

instance Monoid Tx where
  mempty = Tx 1

-----------------
-- Transmitter --
-----------------
data TxFsm = TxStart | TxSend
  deriving (NFDataX, Generic)

data Transmitter = Transmitter
  { _txSer  :: S.Serializer 10 Bit
  , _txBaud :: Unsigned 16
  , _txCtr  :: Unsigned 16
  , _txFsm  :: TxFsm
  }
  deriving (NFDataX, Generic)
makeLenses ''Transmitter

mkTransmitter :: Unsigned 16 -> Transmitter
mkTransmitter b = Transmitter
  { _txSer  = S.mkSerializer 0 S.R
  , _txBaud = b
  , _txCtr  = 0
  , _txFsm  = TxStart
  }

transmit :: Byte -> RWS r Tx Transmitter Bool
transmit byte = use txFsm >>= \case
  TxStart -> do
    zoom txSer $ S.give $ bv2v $ frame byte
    txCtr .= 0
    txFsm .= TxSend
    return False
  TxSend -> do
    zoom txSer S.peek >>= tell . Tx
    baud <- use txBaud
    ctrDone <- uses txCtr (== baud)
    txCtr %= C.incrementUnless (== baud)
    if ctrDone
      then do
        zoom txSer S.serialize
        serEmpty <- zoom txSer S.empty
        when serEmpty $ txFsm .= TxStart
        return serEmpty
      else return False
      
frame :: Byte -> BitVector 10
frame b = (1 :: BitVector 1) ++# b ++# (0 :: BitVector 1)

--------------
-- Receiver --
--------------
data RxFsm = RxIdle | RxStart | RxRecv | RxStop
  deriving (NFDataX, Generic)

data Receiver = Receiver
  { _rxDes  :: S.Deserializer 8 Bit
  , _rxBaud :: Unsigned 16
  , _rxCtr  :: Unsigned 16
  , _rxFsm  :: RxFsm
  }
  deriving (NFDataX, Generic)
makeLenses ''Receiver

mkReceiver :: Unsigned 16 -> Receiver
mkReceiver b = Receiver
  { _rxDes  = S.mkDeserializer 0 S.L
  , _rxBaud = b
  , _rxCtr  = 0
  , _rxFsm  = RxIdle
  }

receive :: Monoid w => RWS Rx w Receiver (Maybe Byte)
receive = use rxFsm >>= \case
  RxIdle ->  do
    rxLow <- asks $ (== low) . unRx
    when rxLow $ do
      rxCtr %= C.increment
      rxFsm .= RxStart
    return Nothing
  RxStart -> do
    rxLow <- asks $ (== low) . unRx
    baudHalf <- uses rxBaud (`shiftR` 1) 
    ctrDone <- uses rxCtr (== baudHalf)
    rxCtr %= C.incrementUnless (== baudHalf)
    when ctrDone $ if rxLow
      then rxFsm .= RxRecv
      else rxFsm .= RxIdle
    return Nothing
  RxRecv -> do
    ctrDone <- countBaud
    when ctrDone $ do
      i <- asks unRx
      zoom rxDes $ S.deserialize i
      full <- zoom rxDes S.full
      when full $ rxFsm .= RxStop
    return Nothing
  RxStop -> do
    ctrDone <- countBaud
    if ctrDone
      then do
        byte <- v2bv <$> zoom rxDes S.get
        zoom rxDes S.clear
        rxFsm .= RxIdle
        return $ Just byte
      else return Nothing
  where
    countBaud = do
      baud <- use rxBaud
      ctrDone <- uses rxCtr (== baud)
      rxCtr %= C.incrementUnless (== baud)
      return ctrDone

----------
-- Uart --
----------
data Uart = Uart
  { _receiver    :: Receiver
  , _transmitter :: Transmitter
  }
  deriving (NFDataX, Generic)
makeLenses ''Uart

mkUart :: Unsigned 16 -> Uart
mkUart baud = Uart
  { _receiver    = mkReceiver baud
  , _transmitter = mkTransmitter baud
  }

read :: Monoid w => RWS Rx w Uart (Maybe Byte)
read = zoom receiver receive

write :: Byte -> RWS r Tx Uart Bool
write = zoom transmitter . transmit
