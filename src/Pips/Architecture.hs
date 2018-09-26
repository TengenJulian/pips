{-# LANGUAGE Arrows #-}
module Pips.Architecture
  ( module Pips.Architecture
  ) where

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Vector as V

import           FRP.Yampa

import Pips.Common
import Pips.Components
import Pips.Instruction
import Pips.Assembler

-- o is for data. Fairly arbitrairy, but it does prevent names clashes.
data ArchitectureComp = ArchitectureComp {
  dMemory :: Seq UInt
  , dRegister :: Seq UInt
  , dInstructions :: V.Vector Instruction
  , dMemoryChange :: Maybe Int
  , dRegisterChange :: Maybe Int
  , dLineNum :: Maybe Int
  , dDebug :: String
  } deriving (Show, Eq)

init16x16 :: [DataEntry UInt] -> [DataEntry UInt] -> [Instruction] -> ArchitectureComp
init16x16 reg mem inst = ArchitectureComp {
  dMemory = initMem emptyData mem
  , dRegister = initMem emptyData reg
  , dInstructions   = V.fromList inst
  , dMemoryChange   = Nothing
  , dRegisterChange = Nothing
  , dLineNum = Nothing
  , dDebug = ""
  }
  where emptyData = S.fromList $ replicate 16 0

architecture :: ArchitectureComp -> V.Vector Int -> SF Bool ArchitectureComp
architecture ArchitectureComp {dMemory = mem, dRegister = reg, dInstructions = insts} endLabelMap =
  let register = regMem reg
      im = instMem insts
      memory = mainMem mem

  in proc debug -> do
    c <- clock -< Rising
    rec
      (pc, inst) <- im -< newPc
      cont     <- control -< (c, inst)

      let done = V.null insts || fromIntegral pc >= V.length insts

      regComp  <- register    -< (cont, rs inst, rt inst, rd inst, writeBack)

      regSrc2        <- aluSrcMutex -< (cont, regB regComp, immediate inst, address inst)
      aluOp'         <- aluControl  -< (opCode inst, aluOp inst)
      (result, zero) <- alu         -< (aluOp', regA regComp, regSrc2)

      memComp <- memory -< (cont, result, regB regComp)
      writeBack <- writebackMutex -< (cont, result, memOutput memComp)

      let nextPc = if done then pc else pc + 1

      newPc    <- delay 10 0 <<< pcMutex  -< (cont, zero, address inst, nextPc, address inst, regA regComp)
      prevDone <- delay 20 (V.null insts) -< returnA done

    let ln | prevDone  = Nothing
           | otherwise =
             case (endLabelMap V.!? endIndex, insts V.!? fromIntegral pc) of
               (Just l, _)     -> Just l
               (_, Just inst') -> Just $ lineNum inst'
               _               -> Nothing
           where endIndex  = fromIntegral pc - V.length insts

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
            , "done: " ++ show done
            , "ln: " ++ show ln
            ]

    returnA -< ArchitectureComp {
      dMemory           = memData memComp
      , dMemoryChange   = deltaMem memComp
      , dRegister       = regData regComp
      , dRegisterChange = deltaReg regComp
      , dInstructions   = insts
      , dLineNum        = ln
      , dDebug          = if debug then debugMsg else ""
      }
