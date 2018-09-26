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

data ArchComp = ArchComp {
  archMem :: Seq UInt
  , archReg :: Seq UInt
  , archInsts :: V.Vector Instruction
  , archMemChange :: Maybe Int
  , archRegChange :: Maybe Int
  , archLineNum :: Maybe Int
  , archDebug :: String
  } deriving (Show, Eq)

init16x16 :: [DataEntry UInt] -> [DataEntry UInt] -> [Instruction] -> ArchComp
init16x16 reg mem inst = ArchComp {
  archMem         = initMem emptyData mem
  , archReg       = initMem emptyData reg
  , archInsts     = V.fromList inst
  , archMemChange = Nothing
  , archRegChange = Nothing
  , archLineNum   = Nothing
  , archDebug     = ""
  }
  where emptyData = S.fromList $ replicate 16 0

architecture :: ArchComp -> V.Vector Int -> SF Bool ArchComp
architecture ArchComp {archMem = mem, archReg = reg, archInsts = insts} endLabelMap =
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

    returnA -< ArchComp {
      archMem         = memData memComp
      , archMemChange = deltaMem memComp
      , archReg       = regData regComp
      , archRegChange = deltaReg regComp
      , archInsts     = insts
      , archLineNum   = ln
      , archDebug     = if debug then debugMsg else ""
      }
