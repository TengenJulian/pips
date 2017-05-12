{-# LANGUAGE Arrows #-}
module Pips.Architecture
  ( module Pips.Architecture
  ) where

import FRP.Yampa

import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Pips.Components
import Pips.Instruction
import Pips.Assembler

-- o is for data. Fairly arbitrairy, but it does prevent names clashes.
data ArchitectureComp = ArchitectureComp {
  dMemory :: Seq Int
  , dRegister :: Seq Int
  , dInstructions :: Seq Instruction
  , dMemoryChange :: Maybe Int
  , dRegisterChange :: Maybe Int
  , dLineNum :: Int
  , dDebug :: String
  } deriving (Show, Eq)

init16x16 :: [DataEntry] -> [DataEntry] -> [Instruction] -> ArchitectureComp
init16x16 reg mem inst = ArchitectureComp {
  dMemory = initMem emptyData mem
  , dRegister = initMem emptyData reg
  , dInstructions   = S.fromList inst
  , dMemoryChange   = Nothing
  , dRegisterChange = Nothing
  , dLineNum = 0
  , dDebug = ""
  }
  where emptyData = S.fromList $ replicate 16 0

architecture :: ArchitectureComp -> SF Bool ArchitectureComp
architecture ArchitectureComp {dMemory = mem, dRegister = reg, dInstructions = insts} =
  let register = regMem reg
      im = instMem insts
      memory = mainMem mem
  in proc debug -> do
    c <- clock -< Rising
    rec
      (pc, inst) <- im -< newPc
      cont     <- control -< (c, inst)

      regComp  <- register    -< (cont, rs inst, rt inst, rd inst, writeBack)

      regSrc2        <- aluSrcMutex -< (cont, regB regComp, immediate inst, address inst)
      aluOp'         <- aluControl  -< (opCode inst, aluOp inst)
      (result, zero) <- alu         -< (aluOp', regA regComp, regSrc2)

      memComp <- memory -< (cont, result, regB regComp)
      writeBack <- writebackMutex -< (cont, result, memOutput memComp)

      newPc    <- delay 10 0 <<< pcMutex -< (cont, zero, address inst, pc + 1, address inst, regA regComp)

    let debugMsg = unlines [
            "Clock: " ++ show c
            , show inst
            , show cont
            , show regComp
            , show memComp
            , "alu result: " ++ show result
            , "writeBack: " ++ show writeBack
            , "program counter: " ++ show pc
            , "new program counter: " ++ show newPc
            ]

    returnA -< ArchitectureComp {
      dMemory           = memData memComp
      , dMemoryChange   = deltaMem memComp
      , dRegister       = regData regComp
      , dRegisterChange = deltaReg regComp
      , dInstructions   = insts
      , dLineNum        = lineNum (S.index insts newPc)
      , dDebug          = if debug then debugMsg else ""
      }
