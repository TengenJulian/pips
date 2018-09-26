module Pips.Assembler
  ( module Pips.Assembler
  , Pips.Parser.DataEntry (..)
  ) where

import qualified Data.Map.Lazy as M

import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Text.Parsec (parse)

import Pips.Parser
import Pips.Instruction

data Assembled = Assembled
  { asmdRegData :: [DataEntry]
  , asmdMemData :: [DataEntry]
  , asmdInsts :: [Instruction]
  , asmdEndLabelMapping :: V.Vector Int
  }

removeRegAlias :: M.Map String Int -> RegName -> RegName
removeRegAlias _ r@(RegNum _) = r
removeRegAlias entries (RegAlias name) =
  case M.lookup name entries of
    Just r -> RegNum r
    _      -> error ("Register $" ++ name ++ " not defined.")

removeMemAlias :: M.Map String Int -> MemAddr -> MemAddr
removeMemAlias _ m@(MemAddrNum _) = m
removeMemAlias entries (MemAddrAlias name) =
  case M.lookup name entries of
    Just a -> MemAddrNum a
    _      -> error ("Memory Address " ++ name ++ " not defined.")

removeLabelAlias :: Int -> [Int] -> M.Map String Int -> M.Map String Int -> Label -> Label
removeLabelAlias _ _ _ _ l@(LabelNum _) = l
removeLabelAlias len lineNums endLabels labels (LabelName name) =
  case (M.lookup name labels, M.lookup name endLabels) of
    (Just l, _) -> LabelNum (fromMaybe len (findIndex (l <) lineNums))
    (_, Just i) -> LabelNum i
    _           -> error ("Label " ++ name ++ " not defined.")

isInstruction :: Token -> Bool
isInstruction ins = case ins of
  CommentToken _ _ -> False
  LabelToken _ _   -> False
  _                -> True

-- | This function removes memory/register aliases and converts jump targets in source lines,
-- to jump target in PC offset.
removeAliases :: [DataEntry] -> [DataEntry] -> [Token] -> (V.Vector Int, [Token])
removeAliases regData memData tokens = (endLabelMapping, map f ins)
  where labels = [l | l@(LabelToken _ _) <- tokens]
        ins = filter isInstruction tokens

        rra = removeRegAlias (M.fromList [(name, r) | DataEntry r (Just name) _ <- regData])
        rma = removeMemAlias (M.fromList [(name, a) | DataEntry a (Just name) _ <- memData])

        lines' = map tokenLineNum ins
        len = length lines'

        lastInLineNum | null ins = 0
                      | otherwise = tokenLineNum $ last $ ins

        endLabels = [lt | lt@(LabelToken l name) <- labels, l > lastInLineNum]
        endLabelMapping = V.fromList [l | LabelToken l name <- endLabels]

        rla = removeLabelAlias len lines'
          (M.fromList $ [(name, len + i) | (i, (LabelToken l name)) <- zip [0..] endLabels])
          (M.fromList [(name, l) | LabelToken l name <- labels, l < lastInLineNum])

        f (BranchToken l name r1 r2 label) = BranchToken l name (rra r1) (rra r2) (rla label)
        f (Reg3Token l name r1 r2 r3)      = Reg3Token   l name (rra r1) (rra r2) (rra r3)
        f (MemOpToken l name r1 m1 r2)     = MemOpToken  l name (rra r1) (rma m1) (rra r2)
        f (Reg2iToken l name r1 r2 v)      = Reg2iToken  l name (rra r1) (rra r2) v

        f (LuiToken l r1 v) = LuiToken l (rra r1) v

        f (JrToken l r1)   = JrToken l (rra r1)
        f (JToken l label) = JToken  l (rla label)

        f token
          | isInstruction token = error ("preprocess Token not implemented for " ++ show token)
          | otherwise           = token

reg3Assemble :: Token -> Instruction
reg3Assemble (Reg3Token l _ (RegNum r1) (RegNum r2) (RegNum r3)) =
  nop {instType = R, opCode = ROpc, rd = r1, rs = r2, rt = r3, lineNum = l}
reg3Assemble _ = error "Applying reg3Assemble to a non Reg3Token" 

branchAssemble :: Token -> Instruction
branchAssemble (BranchToken l _ (RegNum r1) (RegNum r2) (LabelNum offset)) =
  nop {instType = I, rs = r1, rt = r2, address = offset, lineNum = l}
branchAssemble _ = error "Applying branchAssembly to a non BranchToken"

memOpAssemble :: Token -> Instruction
memOpAssemble (MemOpToken l _ (RegNum r1) (MemAddrNum m1) (RegNum r2)) =
  nop {instType = I, rt = r1, immediate = m1, rs = r2, lineNum = l}
memOpAssemble _ = error "Applying memOpAssemble to a non MemOpToken"

assembleToken :: Token -> Instruction
assembleToken t@(Reg3Token _ MulN _ _ _) = (reg3Assemble t) {aluOp = MulOp}
assembleToken t@(Reg3Token _ AddN _ _ _) = (reg3Assemble t) {aluOp = AddOp}
assembleToken t@(Reg3Token _ SubN _ _ _) = (reg3Assemble t) {aluOp = SubOp}
assembleToken t@(Reg3Token _ AndN _ _ _) = (reg3Assemble t) {aluOp = AndOp}
assembleToken t@(Reg3Token _ XorN _ _ _) = (reg3Assemble t) {aluOp = XorOp}
assembleToken t@(Reg3Token _  OrN _ _ _) = (reg3Assemble t) {aluOp = OrOp}
assembleToken t@(Reg3Token _ SltN _ _ _) = (reg3Assemble t) {aluOp = SltOp}

assembleToken t@(BranchToken _ BeqN _ _ _) = (branchAssemble t) {opCode = BeqOpc}
assembleToken t@(BranchToken _ BneN _ _ _) = (branchAssemble t) {opCode = BneOpc}

assembleToken t@(MemOpToken _ SwN _ _ _) = (memOpAssemble t) {opCode = SwOpc}
assembleToken t@(MemOpToken _ LwN _ _ _) = (memOpAssemble t) {opCode = LwOpc}

assembleToken (Reg2iToken l AddiN (RegNum r1) (RegNum r2) imm) =
  nop {instType = I, opCode = AddiOpc, rt = r1, immediate=imm, rs = r2, lineNum = l}

assembleToken (Reg2iToken l name (RegNum r1) (RegNum r2) imm) =
  let aop = if name == SllN then SllOp else SrlOp in
  nop {rd = r1, immediate=imm, rt = r2, aluOp = aop, opCode = ROpc, lineNum = l}

assembleToken (LuiToken l (RegNum r1) imm) =
  nop {instType = I, opCode = LuiOpc, rt = r1, immediate = imm, lineNum = l}

assembleToken (JrToken l (RegNum r1)) =
  nop {instType = R, opCode = ROpc, aluOp = JrOp, rs = r1, lineNum = l}

assembleToken (JToken l (LabelNum l1)) =
  nop {instType = J, opCode = JOpc, address = l1, lineNum = l}

assembleToken t = error ("Could not assemble token " ++ show t)

showLeft :: Show a => Either a b -> Either String b
showLeft (Right x) = Right x
showLeft (Left err)  = Left (show err)

assemble :: String -> Either String Assembled
assemble source = do
  (regData, memData, tokens) <- showLeft $ parse parseFile "" source
  let (endLabelMapping, tokens') = removeAliases regData memData tokens
      insts = map assembleToken tokens'

  return Assembled
    { asmdRegData = regData
    , asmdMemData = memData
    , asmdInsts = insts
    , asmdEndLabelMapping = endLabelMapping
    }
