module Pips.Common where

import Data.Word
import FRP.Yampa

type UInt = Word32

sizeWord, sizeImm, sizeShamt, sizeAddr, numRegs :: Int

sizeWord  = 32
sizeImm   = 16
sizeAddr  = 26
sizeShamt = 5
numRegs   = 32

timeHalfCycle, timeCycle :: Double

timeHalfCycle = 8
timeCycle = 16

delayCycle :: a -> SF a a
delayCycle = delay timeCycle

delayHalfCycle :: a -> SF a a
delayHalfCycle = delay timeHalfCycle
