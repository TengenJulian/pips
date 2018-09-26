module Pips.Assembler
  ( module Pips.Assembler
  , Pips.Parser.DataEntry (..)
  ) where

import qualified Data.Map.Lazy as M

import           Data.List (findIndex, intercalate)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import           Text.Parsec (parse, ParseError)

import Pips.Common
import Pips.Instruction
import Pips.Parser

data Assembled = Assembled
  { asmdRegData :: [DataEntry UInt]
  , asmdMemData :: [DataEntry UInt]
  , asmdInsts :: [Instruction]
  , asmdEndLabelMapping :: V.Vector Int
  }

data AssembleError =
  ParsecError ParseError
  | InvalidSignedImm Int Int
  | InvalidImm Int Int
  | InvalidShamt Int Int
  | InvalidAddr Int Int
  | InvalidDataEntry Int (Maybe String) Int
  deriving (Eq)

maxNBits :: Int -> Int
maxNBits numBits = iterate ((+ 1) . (2 *)) 1 !! numBits

validSignedBounds, validUnsignedBounds :: Int -> (Int, Int)
validSignedBounds   numBits = ((-1) * 2 ^ (numBits - 1), maxNBits numBits)
validUnsignedBounds numBits = (0, maxNBits numBits)

validSigned, validUnsigned :: Int -> Int -> Bool
validSigned numBits n = l <= n && n <= r
  where  (l, r) = validSignedBounds numBits

validUnsigned numBits n = l <= n && n <= r
  where (l, r) = validUnsignedBounds numBits

expectedRangeMsg :: (Int, Int) -> String
expectedRangeMsg (l, r) = "Expected value to be in inclusive range " ++ show l ++ "-" ++ show r

instance Show AssembleError where
  show (ParsecError e) = show e
  show (InvalidImm l n) = intercalate " "
    [ "Invalid signed immediate value, "
    , show n, ", at line", show l ++ "."
    , expectedRangeMsg $ validSignedBounds 16
    ]
  show (InvalidSignedImm l n) = intercalate " "
    [ "Invalid immediate value, "
    , show n, ", at line", show l ++ "."
    , expectedRangeMsg $ validUnsignedBounds 16
    ]
  show (InvalidShamt l n) = intercalate " "
    [ "Invalid shift amount, "
    , show n, ", at line", show l ++ "."
    , expectedRangeMsg $ validUnsignedBounds 5
    ]
  show (InvalidAddr l n) = intercalate " "
    [ "Invalid jump addr, "
    , show n, ", at line", show l ++ "."
    , expectedRangeMsg $ validUnsignedBounds 26
    ]
  show (InvalidDataEntry i alias val) = intercalate " "
    [ "Invalid Data entry value, "
    , show val, ", at index", show i
      ++ aliasMsg alias ++ "."
    , expectedRangeMsg $ validSignedBounds 32
    ]
    where aliasMsg (Just al) = ", for alias " ++ al
          aliasMsg _            = ""

removeRegAlias :: M.Map String Int -> RegName -> RegName
removeRegAlias _ r@(RegNum _) = r
removeRegAlias entries (RegAlias name) =
  case M.lookup name entries of
    Just r -> RegNum (fromIntegral r)
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
    (Just l, _) -> LabelNum . fromIntegral $ fromMaybe len (findIndex (l <) lineNums)
    (_, Just i) -> LabelNum . fromIntegral $ i
    _           -> error ("Label " ++ name ++ " not defined.")

isInstruction :: Token -> Bool
isInstruction ins = case ins of
  CommentToken _ _ -> False
  LabelToken _ _   -> False
  _                -> True

-- | This function removes memory/register aliases and converts jump targets in source lines,
-- to jump target in PC offset.
removeAliases :: [DataEntry n] -> [DataEntry n] -> [Token] -> (V.Vector Int, [Token])
removeAliases regData memData tokens = (endLabelMapping, map f ins)
  where labels = [l | l@(LabelToken _ _) <- tokens]
        ins = filter isInstruction tokens

        rra = removeRegAlias (M.fromList [(name, r) | DataEntry r (Just name) _ <- regData])
        rma = removeMemAlias (M.fromList [(name, a) | DataEntry a (Just name) _ <- memData])

        lines' = map tokenLineNum ins
        len = length lines'

        lastInLineNum | null ins = 0
                      | otherwise = tokenLineNum $ last ins

        endLabels = [lt | lt@(LabelToken l _) <- labels, l > lastInLineNum]
        endLabelMapping = V.fromList [l | LabelToken l _ <- endLabels]

        rla = removeLabelAlias len lines'
          (M.fromList [(name, len + i) | (i, LabelToken _ name) <- zip [0..] endLabels])
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

convertImm, convertSignedImm, convertShamt :: Int -> Int -> Either AssembleError UInt
convertSignedImm l x
  | validSigned 16 x = Right $ fromIntegral x
  | otherwise        = Left $ InvalidSignedImm l x

convertImm l x
  | validUnsigned 16 x = Right $ fromIntegral x
  | otherwise          = Left $ InvalidImm l x

convertShamt l x
  | validUnsigned 5 x = Right $ fromIntegral x
  | otherwise         = Left $ InvalidShamt l x

convertAddr :: Int -> Int -> Either AssembleError UInt
convertAddr l addr
  | validSigned 26 addr = Right $ fromIntegral addr
  | otherwise = Left $ InvalidAddr l addr

convertEntries :: [DataEntry Int] -> Either AssembleError [DataEntry UInt]
convertEntries = mapM f
  where f (DataEntry i alias val)
          | validSigned 32 val = Right $ DataEntry i alias (fromIntegral val)
          | otherwise          = Left  $ InvalidDataEntry i alias val

reg3Assemble :: Token -> Either AssembleError Instruction
reg3Assemble (Reg3Token l _ (RegNum r1) (RegNum r2) (RegNum r3)) =
  return nop {instType = R, opCode = ROpc, rd = r1, rs = r2, rt = r3, lineNum = l}
reg3Assemble _ = error "Applying reg3Assemble to a non Reg3Token"

branchAssemble :: Token -> Either AssembleError Instruction
branchAssemble (BranchToken l _ (RegNum r1) (RegNum r2) (LabelNum offset)) =
  (\o -> nop {instType = I, rs = r1, rt = r2, address = o, lineNum = l}) <$> convertSignedImm l offset
branchAssemble _ = error "Applying branchAssembly to a non BranchToken"

memOpAssemble :: Token -> Either AssembleError Instruction
memOpAssemble (MemOpToken l _ (RegNum r1) (MemAddrNum m1) (RegNum r2)) =
  (\imm -> (nop {instType = I, rt = r1, immediate = imm, rs = r2, lineNum = l})) <$> convertSignedImm l m1
memOpAssemble _ = error "Applying memOpAssemble to a non MemOpToken"

assembleToken :: Token -> Either AssembleError Instruction
assembleToken t@(Reg3Token _ MulN _ _ _) = (\inst -> inst {aluOp = MulOp}) <$> reg3Assemble t
assembleToken t@(Reg3Token _ AddN _ _ _) = (\inst -> inst {aluOp = AddOp}) <$> reg3Assemble t
assembleToken t@(Reg3Token _ SubN _ _ _) = (\inst -> inst {aluOp = SubOp}) <$> reg3Assemble t
assembleToken t@(Reg3Token _ AndN _ _ _) = (\inst -> inst {aluOp = AndOp}) <$> reg3Assemble t
assembleToken t@(Reg3Token _ XorN _ _ _) = (\inst -> inst {aluOp = XorOp}) <$> reg3Assemble t
assembleToken t@(Reg3Token _  OrN _ _ _) = (\inst -> inst {aluOp = OrOp} ) <$> reg3Assemble t
assembleToken t@(Reg3Token _ SltN _ _ _) = (\inst -> inst {aluOp = SltOp}) <$> reg3Assemble t

assembleToken t@(BranchToken _ BeqN _ _ _) = (\inst -> inst {opCode = BeqOpc}) <$> branchAssemble t
assembleToken t@(BranchToken _ BneN _ _ _) = (\inst -> inst {opCode = BneOpc}) <$> branchAssemble t

assembleToken t@(MemOpToken _ SwN _ _ _) = (\inst -> inst {opCode = SwOpc}) <$> memOpAssemble t
assembleToken t@(MemOpToken _ LwN _ _ _) = (\inst -> inst {opCode = LwOpc}) <$> memOpAssemble t

assembleToken (Reg2iToken l AddiN (RegNum r1) (RegNum r2) imm') =
  (\imm -> nop {instType = I, opCode = AddiOpc, rt = r1, immediate = imm, rs = r2, lineNum = l})
  <$> convertSignedImm l imm'

assembleToken (Reg2iToken l name (RegNum r1) (RegNum r2) imm) =
  let aop = if name == SllN then SllOp else SrlOp in
  (\shamt' -> nop {rd = r1, immediate = shamt', rt = r2, aluOp = aop, opCode = ROpc, lineNum = l})
  <$> convertShamt l imm

assembleToken (LuiToken l (RegNum r1) imm') =
  (\imm -> nop {instType = I, opCode = LuiOpc, rt = r1, immediate = imm, lineNum = l})
  <$> convertImm l imm'

assembleToken (JrToken l (RegNum r1)) =
  return nop {instType = R, opCode = ROpc, aluOp = JrOp, rs = r1, lineNum = l}

assembleToken (JToken l (LabelNum l1)) =
  (\addr -> nop {instType = J, opCode = JOpc, address = addr, lineNum = l}) <$> convertAddr l l1

assembleToken t = error ("Could not assemble token " ++ show t)

mapLeft :: (e -> e') -> Either e b -> Either e' b
mapLeft _ (Right x) = Right x
mapLeft f (Left err)  = Left (f err)

assemble :: String -> Either AssembleError Assembled
assemble source = do
  (regData, memData, tokens') <- mapLeft ParsecError $ parse parseFile "" source
  let (endLabelMapping, tokens) = removeAliases regData memData tokens'

  Assembled
    <$> convertEntries regData
    <*> convertEntries memData
    <*> mapM assembleToken tokens
    <*> return endLabelMapping
