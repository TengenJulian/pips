{-# LANGUAGE Arrows #-}
{-# LANGUAGE MagicHash #-}
module Pips.Components
  ( module Pips.Components
  ) where

import           Data.Bits
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Vector as V

import           FRP.Yampa

import Pips.Common
import Pips.Instruction
import Pips.Assembler

data Clock = Rising | Falling deriving (Eq, Show, Enum)

flipClock :: Clock -> Clock
flipClock Rising = Falling
flipClock Falling = Rising

delayHalfClock :: a -> SF a a
delayHalfClock = delay 10

-- TODO: This clock might be broken, as it has no time delay.
-- This might be the reason, why 2 reacts separate are needed
-- for a full clock cycle, where each react update the state
-- of the clock.
clock :: SF Clock Clock
clock = loopPre Falling $ proc (_, state) ->
  returnA -< (flipClock state, flipClock state)

alu :: SF (AluOp, UInt, UInt) (UInt, Bool)
alu = proc (aluOp', left', right') -> do
  let result = case aluOp' of
        MulOp -> left' * right'
        AddOp -> left' + right'
        SubOp -> left' - right'
        AndOp -> left' .&. right'
        OrOp  -> left' .|. right'
        SllOp -> left' `shiftL` fromIntegral right'
        SrlOp -> left' `shiftR` fromIntegral right'
        SltOp -> fromIntegral . fromEnum $ left' < right'
        XorOp -> left' `xor` right'
        LuiOp -> left' .|. shiftL right' 16
        _     -> left'

  returnA -< (result, result == 0)

data Control = Control {
  aluSrc :: AluSrc
  , branchAct :: BranchAct
  , memAct :: DataAct
  , regAct :: DataAct
  , regWriteDst :: RegDst
  , regWriteData :: RegData
  } deriving (Show, Eq)

data BranchAct = PC4     | Jump  | Branch    | JumpReg deriving (Show, Eq)
data AluSrc    = Shamt   | Reg   | Immediate | Address deriving (Show, Eq)
data RegDst    = Rs      | Rt    | Rd deriving (Show, Eq)
data RegData   = MemData | AluOut deriving (Show, Eq)
data DataAct   = Write   | Read deriving (Show, Eq)

control :: SF (Clock, Instruction) Control
control = proc (c, inst) -> do
  let branchAct'
        | opCode inst `elem` [BeqOpc, BneOpc] = Branch
        | opCode inst == JOpc  = Jump
        | opCode inst == JrOpc = JumpReg
        | otherwise            = PC4

      -- how are we going to deal with the difference between immediate, shamt, address
      -- they are different in the sense that they use different number of bits.
      aluSrc'
        | instType inst == R && aluOp inst `elem` [SllOp, SrlOp] = Shamt
        | instType inst == I && opCode inst == JOpc = Address
        | instType inst == I = Immediate
        | otherwise = Reg

      memAct'
        | c == Falling && opCode inst == SwOpc = Write
        | otherwise = Read

      regAct'
        | c == Rising = Read
        | instType inst == R && aluOp inst /= JrOp = Write
        | instType inst == I && opCode inst `elem` [AddiOpc, LuiOpc, LwOpc] = Write
        | otherwise = Read

      regWriteSrc'
        | instType inst == I = Rt
        | otherwise          = Rd

      regWriteData'
        | instType inst == I && opCode inst == LwOpc = MemData
        | otherwise = AluOut

  returnA -< Control aluSrc' branchAct' memAct' regAct' regWriteSrc' regWriteData'

instMem :: V.Vector Instruction -> SF UInt (UInt, Instruction)
instMem mem = proc pc -> do
  newPc <- delay 10 0 -< pc
  returnA -< (newPc, fromMaybe nop (mem V.!? fromIntegral newPc))

data RegComp = RegComp {
    regData :: Seq UInt
    , regA :: UInt
    , regB :: UInt
    , deltaReg :: Maybe Int
    } deriving (Eq, Show)

initMem :: Seq UInt -> [DataEntry UInt] -> Seq UInt
initMem = foldl (\mem' (DataEntry loc _ val) -> S.update loc (fromIntegral val) mem')

safeGet :: Seq a -> a -> UInt -> a
safeGet xs x i
  | i < 0 = x
  | fromIntegral i >= length xs = x
  | otherwise = S.index xs (fromIntegral i)

regMem :: Seq UInt -> SF (Control, UInt, UInt, UInt, UInt) RegComp
regMem mem = proc (cont, rs', rt', rd', writeData) -> do
  let rDst      = if regWriteDst cont == Rt then rt' else rd'
      doWrite   = regAct cont == Write && rDst /= 0

  rec mem'  <- delay 10 mem -< if doWrite then S.update (fromIntegral rDst) writeData mem' else mem'

  deltaReg' <- delay 10 Nothing -<  if doWrite then Just (fromIntegral rDst) else Nothing

  let (ra, rb)
        | aluSrc cont == Shamt = (rt', rs')
        | otherwise            = (rs' ,rt')

  returnA -< RegComp mem' (safeGet mem' 0xFFFFFFFF ra) (safeGet mem' 0xFFFFFFFF rb) deltaReg'

data MemComp = MemComp {
    memData :: Seq UInt
    , memOutput :: UInt
    , deltaMem :: Maybe Int
    } deriving (Eq, Show)

mainMem :: Seq UInt -> SF (Control, UInt, UInt) MemComp
mainMem mem = proc (cont, address', writeData) -> do
  let doWrite = memAct cont == Write

  rec
    mem'   <- delay 10 mem -< if doWrite then S.update (fromIntegral address') writeData mem' else mem'
    output <- delay 10 0 -< if doWrite then output else safeGet mem' 0xFFFFFFFF address'

  deltaMem' <- delay 10 Nothing -< if doWrite then Just (fromIntegral address') else Nothing

  returnA -< MemComp mem' output deltaMem'

aluSrcMutex :: SF (Control, UInt, UInt, UInt) UInt
aluSrcMutex = proc (cont, regb, imm, addr) ->
  returnA -< case aluSrc cont of
    Reg       -> regb
    Immediate -> imm
    Address   -> addr
    Shamt     -> imm

writebackMutex :: SF (Control, UInt, UInt) UInt
writebackMutex = proc (cont, aluOutput, memoryData) ->
  returnA -< case regWriteData cont of
    MemData -> memoryData
    AluOut  -> aluOutput

pcMutex :: SF (Control, Bool, UInt, UInt, UInt, UInt) UInt
pcMutex = proc (cont, zero, branch, pc4, jump, jumpReg) ->
  returnA -< case branchAct cont of
    PC4     -> pc4
    Jump    -> jump
    JumpReg -> jumpReg
    Branch  -> if zero then branch else pc4

aluControl :: SF (OpCode, AluOp) AluOp
aluControl = proc (opCode', aluOp') -> do
  let result
        | opCode' == ROpc    = aluOp'
        | opCode' == AddiOpc = AddOp
        | opCode' == BeqOpc  = SubOp
        | opCode' == BneOpc  = XorOp
        | opCode' == SwOpc   = AddOp
        | opCode' == LwOpc   = AddOp
        | opCode' == LuiOpc  = LuiOp
        | otherwise          = SllOp
  returnA -< result

counter :: SF () Int
counter = proc _ -> do
  rec x <- delay 10 0 -< x + 1

  returnA -< x

clockSF :: SF () Clock
clockSF = counter >>> arr (toEnum . (`mod` 2))

testMem :: [Int] -> SF (Clock, Int ) Int
testMem mem = loopPre (mem, 0) $ proc ((c, i),(mem', output)) -> do
  let result
        | c == Rising = (mem' !! i, (mem', mem' !! i))
        | otherwise = (output , (mem', output))
  returnA -< result


testCircuit :: Eq a => [a] -> SF a b -> [b]
testCircuit xs sf = embed sf steps
  where steps = deltaEncode 10 xs
