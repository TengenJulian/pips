module Pips.Assembler
  ( module Pips.Assembler
  , Pips.Parser.DataEntry (..)
  ) where

import           Control.Monad ((<=<))

import qualified Data.Map.Lazy as M

import           Data.List (findIndex)
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
  | UndefinedRegAlias Int String
  | UndefinedMemAlias Int String
  | UndefinedJumpLabel Int String
  | InvalidRegEntry Int (Maybe String) Int
  | InvalidMemEntry Int (Maybe String) Int
  | NonZeroRegZero
  | InvalidReg Int UInt
  | InitializingInvalidReg Int
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
expectedRangeMsg (l, r) = "Expected value to be in inclusive range " ++ show l ++ "-" ++ show r ++ "."

instance Show AssembleError where
  show (ParsecError e) = show e
  show (InvalidImm l n) =
    "Invalid signed immediate value, "
    ++ show n ++ ", at line " ++ show l ++ ". "
    ++ expectedRangeMsg (validSignedBounds 16)
  show (InvalidSignedImm l n) =
    "Invalid immediate value, "
    ++ show n ++ ", at line " ++ show l ++ ". "
    ++ expectedRangeMsg (validUnsignedBounds 16)
  show (InvalidShamt l n) =
    "Invalid shift amount, "
    ++ show n ++ ", at line " ++ show l ++ ". "
    ++ expectedRangeMsg (validUnsignedBounds 5)
  show (InvalidAddr l n) =
    "Invalid jump addr, "
    ++ show n ++ " ++ at line " ++ show l ++ ". "
    ++ expectedRangeMsg (validUnsignedBounds 26)
  show (InvalidMemEntry i alias val) =
    "Invalid Data entry value, "
    ++ show val ++ aliasMsg alias
    ++ expectedRangeMsg (validSignedBounds 32)
    where aliasMsg (Just al) = ", for memory address $" ++ al ++ ". "
          aliasMsg _         = ", for memory address $" ++ show i ++ ". "
  show (InvalidRegEntry i alias val) =
    "Invalid Data entry value, "
    ++ show val ++ aliasMsg alias
    ++ expectedRangeMsg (validSignedBounds 32)
    where aliasMsg (Just al) = ", for reg $" ++ al ++ ". "
          aliasMsg _         = ", for reg $" ++ show i ++ ". "
  show (UndefinedRegAlias l name) =
    "Undefined reg alias, "
    ++ name ++ ", at line " ++ show l ++ "."
  show (UndefinedMemAlias l name) =
    "Undefined mem alias, "
    ++ name ++ ", at line " ++ show l ++ "."
  show (UndefinedJumpLabel l name) =
    "Undefined jump label, "
    ++ name ++ ", at line " ++ show l ++ "."
  show (InitializingInvalidReg i) =
    "Initializing invalid register $" ++ show i ++ ". "
    ++ "There are only 32 registers."
  show (InvalidReg l i) =
    "Invalid register $" ++ show i
    ++ ", on line " ++ show l ++ ". "
    ++ "There are only 32 registers."
  show NonZeroRegZero = "Register $0 cannot be initialized to a non-zero value."

removeRegAlias :: M.Map String Int -> Int -> RegName -> Either AssembleError RegName
removeRegAlias _ _ r@(RegNum _) = Right r
removeRegAlias entries ln (RegAlias name) =
  case M.lookup name entries of
    Just r -> Right . RegNum $ fromIntegral r
    _      -> Left $ UndefinedRegAlias ln name

removeMemAlias :: M.Map String Int -> Int -> MemAddr -> Either AssembleError MemAddr
removeMemAlias _ _ m@(MemAddrNum _) = Right m
removeMemAlias entries ln (MemAddrAlias name) =
  case M.lookup name entries of
    Just a -> Right $ MemAddrNum a
    _      -> Left $ UndefinedMemAlias ln name

removeLabelAlias :: Int -> [Int] -> M.Map String Int -> M.Map String Int -> Int -> Label -> Either AssembleError Label
removeLabelAlias _ _ _ _ _ l@(LabelNum _) = Right l
removeLabelAlias len lineNums endLabels labels ln (LabelName name) =
  case (M.lookup name labels, M.lookup name endLabels) of
    (Just l, _) -> Right . LabelNum . fromIntegral $ fromMaybe len (findIndex (l <) lineNums)
    (_, Just i) -> Right . LabelNum . fromIntegral $ i
    _           -> Left . UndefinedJumpLabel ln $ name

isInstruction :: Token -> Bool
isInstruction ins = case ins of
  CommentToken _ _ -> False
  LabelToken _ _   -> False
  _                -> True

-- | This function removes memory/register aliases and converts jump targets in source code,
-- to jump target in PC offset.
removeAliases :: [DataEntry n] -> [DataEntry n] -> [Token] -> Either AssembleError (V.Vector Int, [Token])
removeAliases regData memData tokens = (,) <$> return endLabelMapping <*> mapM f ins
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

        f (BranchToken l name r1 r2 label) = BranchToken l name <$> rra l r1 <*> rra l r2 <*> rla l label
        f (Reg3Token l name r1 r2 r3)      = Reg3Token   l name <$> rra l r1 <*> rra l r2 <*> rra l r3
        f (MemOpToken l name r1 m1 r2)     = MemOpToken  l name <$> rra l r1 <*> rma l m1 <*> rra l r2
        f (Reg2iToken l name r1 r2 v)      = Reg2iToken  l name <$> rra l r1 <*> rra l r2 <*> return v

        f (LuiToken l r1 v) = LuiToken l <$> rra l r1 <*> return v

        f (JrToken l r1)   = JrToken l <$> rra l r1
        f (JToken l label) = JToken  l <$> rla l label

        f token
          | isInstruction token = error ("preprocess Token not implemented for " ++ show token)
          | otherwise           = Right token

conversionHelper :: (Int -> Bool) -> Int -> e -> Either e UInt
conversionHelper p x e
  | p x       = Right $ fromIntegral x
  | otherwise = Left e

convertImm, convertSignedImm, convertShamt, convertAddr :: Int -> Int -> Either AssembleError UInt

convertSignedImm l x = conversionHelper (validSigned 16) x $ InvalidSignedImm l x
convertImm       l x = conversionHelper (validSigned 16) x $ InvalidImm l x
convertAddr      l x = conversionHelper (validSigned 26) x $ InvalidAddr l x
convertShamt     l x = conversionHelper (validSigned 5)  x $ InvalidShamt l x

convertEntry :: DataEntry Int -> Either (Int, Maybe String, Int) (DataEntry UInt)
convertEntry (DataEntry i alias val)
  | validSigned 32 val = Right $ DataEntry i alias (fromIntegral val)
  | otherwise          = Left (i, alias, val)

mapLeft :: (e -> e') -> Either e b -> Either e' b
mapLeft _ (Right x) = Right x
mapLeft f (Left err)  = Left (f err)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

convertRegEntries :: [DataEntry Int] -> Either AssembleError [DataEntry UInt]
convertRegEntries = mapM (checkRegZero <=< (mapLeft (uncurry3 InvalidRegEntry) . convertEntry))
  where checkRegZero d@(DataEntry i _ val)
          | 0 <= i && i < 32   = Right d
          | i == 0 && val /= 0 = Left NonZeroRegZero
          | otherwise          = Left $ InitializingInvalidReg i

convertMemEntries :: [DataEntry Int] -> Either AssembleError [DataEntry UInt]
convertMemEntries = mapM (mapLeft (uncurry3 InvalidMemEntry) . convertEntry)

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

checkRegs :: Instruction -> Either AssembleError Instruction
checkRegs inst = do
  let checkReg i
        | 0 <= i && i < 32  = Right $ ()
        | otherwise         = Left $ InvalidReg (lineNum inst) i

  checkReg $ rt inst
  checkReg $ rs inst
  checkReg $ rd inst

  return inst

assemble :: String -> Either AssembleError Assembled
assemble source = do
  (regData, memData, tokens') <- mapLeft ParsecError $ parse parseFile "" source
  (endLabelMapping, tokens)   <- removeAliases regData memData tokens'

  Assembled
    <$> convertRegEntries regData
    <*> convertMemEntries memData
    <*> mapM (checkRegs <=< assembleToken) tokens
    <*> return endLabelMapping
