module Pips.Assembler where

import Pips.Instruction
import Text.Parsec
import Text.Parsec.String

import Data.Bits
import Data.Char
import Data.Maybe

import Control.Monad


-- TODO: convert into a 1-pass + post processing by using raw instruction

data Token = InstToken Instruction | CommentToken String | Label String | Other
           | RegToken [DataEntry]  | MemToken [DataEntry]
           deriving (Show, Eq)

type Globals = ([DataEntry], [DataEntry], [(String, Int)])

-- This function parses a wasm containing bips assembly, to instructions
-- and data entries.
assemble :: String -> Either String ([DataEntry], [DataEntry], [Instruction])
assemble input = do
  ls <- findLabels input
  showLeft . parse (wasm ls) "" $ input

showLeft :: Show a => Either a b -> Either String b
showLeft (Right x) = Right x
showLeft (Left err)  = Left (show err)

wasm :: [(String, Int)] -> Parser ([DataEntry], [DataEntry], [Instruction])
wasm labels = do
  startingToken <- many (pRegData <|> pMemData <|> comment)

  let rawmData = [d | MemToken d <- startingToken]
      rawrData = [d | RegToken d <- startingToken]

  when (length rawmData > 1) $ fail "Multiple memory segments defined."
  when (length rawrData > 1) $ fail "Multiple registery segments defined."

  let mData = concat rawmData
      rData = concat rawrData

  let single = comment <|> try jumpLabel <|> (instruction (mData, rData, labels))

  tokens <- single `endBy` (eof<|> spaces)

  return (rData, mData, processLabels [inst | InstToken inst <- tokens])

-- The ugly part of matching a jump label to the proper instruction address
-- This is done by comparing the line number in the source code with actual offset
-- in memory.
--     For example if a jump or a branch instructions says to jump to line n,
-- compare it with the list of instruction and look where they appeared in the source file.
-- It should jump to the instruction i, that appeared on a line number that is the closest to n,
-- but appeared on a line number that is equal or higher than n.
--     Before this stage, the jump address points to the line number in the source code,
-- not to the location of the instruction in the instruction memory.
processLabels :: [Instruction] -> [Instruction]
processLabels insts = map f insts
  where lineNums = zipWith (\inst real -> (lineNum inst, real)) insts [0..] ++ [(maxBound, maxBound)]
        getRealInstAddr li = snd . head . dropWhile ((< li) . fst) $ lineNums
        needsLabel inst = opCode inst `elem` [BeqOpc, BneOpc, JOpc]
        f inst | needsLabel inst = inst {address = getRealInstAddr (immediate inst)}
               | otherwise       = inst

findLabels :: String -> Either String [(String, Int)]
findLabels input = do
  let ilines = zip [0..] . lines $ input
  r <- forM ilines $ \(i, x) -> do
    ls <- getLabels x
    mapM (\l -> return (l, i)) ls

  return $ concat r

getLabels :: String -> Either String [String]
getLabels input = showLeft $ parse p "" input
  where p = do
          let word = many1 (letter <|> digit <|> oneOf ".$-_,#")

          many (pRegData <|> pMemData <|> comment)
          tokens <- many (try jumpLabel
                          <|> (spaces' >> return Other)
                          <|> (word >> return Other)
                          <|> (number >> spaces >> char ':' >> return Other)
                         )
          return [l | Label l <- tokens]

identifier :: Parser String
identifier = do
  l <- letter
  rest <- many (letter <|> digit)

  return (l : rest)

spaces' :: Parser ()
spaces' = skipMany1 space

jumpLabel :: Parser Token
jumpLabel = fmap Label $ do
  l <- identifier
  char ':'
  return l

comment :: Parser Token
comment = fmap CommentToken $ do
  c <- char '#'
  rest <- many (noneOf "\n\r")

  return (c : rest)

comma :: Parser Char
comma = do
  spaces
  c <- char ','
  spaces
  return c

number :: Parser Int
number = do
  prefix <- try (string "0x") <|> try (string "0b") <|> fmap (:[]) digit

  case prefix of
    "0x" -> fmap (read . ("0x"++)) $ many1 hexDigit
    "0b" -> fmap binToDecimal $ many1 (oneOf "01")
    _    -> fmap (read . (prefix ++ )) $ many digit


binToDecimal :: String -> Int
binToDecimal str = foldl (\acc (d, i) -> acc .|. (shiftL d i)) 0 $ zip (reverse bits) [0..]
  where bits = map digitToInt str

data DataEntry = DataEntry Int (Maybe String) Int deriving (Show, Eq)

staticData :: Parser [DataEntry]
staticData = do
  let single = do
        location <- fmap read $ many1 digit
        spaces
        char ':'
        spaces
        string "WORD"
        spaces
        alias <- optionMaybe identifier
        spaces
        value <- number

        return $ DataEntry location alias value

  single `endBy` spaces

pRegData :: Parser Token
pRegData = fmap RegToken $ try (string ".register") >> spaces >> staticData

pMemData :: Parser Token
pMemData = fmap MemToken $ try (string ".memory") >> spaces >> staticData

findAlias :: [DataEntry] -> String -> Maybe Int
findAlias entries alias = listToMaybe [loc | DataEntry loc alias' _ <- entries, alias' == Just alias]

register :: [DataEntry] -> Parser Int
register entries = char '$' >> aliasOrNum entries (\name -> "$" ++ name ++ " is not defined.")

memAddress :: [DataEntry] -> Parser Int
memAddress entries = aliasOrNum entries (\name -> "Address " ++ name ++ " is not defined.")

aliasOrNum :: [DataEntry] -> (String -> String) -> Parser Int
aliasOrNum entries f = do
  name <- optionMaybe identifier
  case name of
    Nothing -> fmap read $ many1 digit
    Just n  ->
      case findAlias entries n of
        Just reg -> return reg
        Nothing  -> fail $ f n

keywordToInstPartial :: String -> Maybe Instruction
keywordToInstPartial "add" = Just $ nop {instType = R, opCode = ROpc, aluOp = AddOp}
keywordToInstPartial "sub" = Just $ nop {instType = R, opCode = ROpc, aluOp = SubOp}
keywordToInstPartial "and" = Just $ nop {instType = R, opCode = ROpc, aluOp = AndOp}
keywordToInstPartial "or"  = Just $ nop {instType = R, opCode = ROpc, aluOp = OrOp }
keywordToInstPartial "sll" = Just $ nop {instType = R, opCode = ROpc, aluOp = SllOp}
keywordToInstPartial "srl" = Just $ nop {instType = R, opCode = ROpc, aluOp = SrlOp}
keywordToInstPartial "slt" = Just $ nop {instType = R, opCode = ROpc, aluOp = SltOp}
keywordToInstPartial "jr"  = Just $ nop {instType = R, opCode = ROpc, aluOp = JrOp }

keywordToInstPartial "addi" = Just $ nop {instType = I, opCode = AddiOpc }
keywordToInstPartial "lui"  = Just $ nop {instType = I, opCode = LuiOpc  }
keywordToInstPartial "lw"   = Just $ nop {instType = I, opCode = LwOpc   }
keywordToInstPartial "sw"   = Just $ nop {instType = I, opCode = SwOpc   }
keywordToInstPartial "beq"  = Just $ nop {instType = I, opCode = BeqOpc  }
keywordToInstPartial "bne"  = Just $ nop {instType = I, opCode = BneOpc  }

keywordToInstPartial "j"    = Just $ nop {instType = J, opCode = JOpc    }
keywordToInstPartial _      = Nothing

instruction :: Globals -> Parser Token
instruction gs = do
  name <- identifier
  let mPartial = keywordToInstPartial $ map toLower name

  lineNum' <- fmap ((+(-1)) . sourceLine) getPosition

  case mPartial of
    Nothing      -> fail $ "Unknown command " ++ name
    Just partial -> spaces >> (fmap InstToken $ completePart gs (partial {lineNum = lineNum'}))

completePart :: Globals -> Instruction -> Parser Instruction
completePart entries inst@(Instruction {instType = R, aluOp=SllOp}) = shifts (rData entries) inst
completePart entries inst@(Instruction {instType = R, aluOp=SrlOp}) = shifts (rData entries) inst

completePart entries inst@(Instruction {instType = R, aluOp=JrOp}) = do
  rs' <- register (rData entries)
  return inst{rs = rs'}

completePart entries inst@(Instruction {instType = R}) = do
  rd' <- register (rData entries)
  comma

  rs' <- register (rData entries)
  comma

  rt' <- register (rData entries)

  return inst{rd = rd', rs = rs', rt = rt'}

completePart entries inst@(Instruction {instType = I, opCode=AddiOpc}) = do
  (rt' ,rs', imm) <- completeImmediate (rData entries)
  return inst{rt = rt', immediate=imm, rs = rs'}

completePart entries inst@(Instruction {instType = I, opCode=LuiOpc}) = do
  rt' <- register (rData entries)
  comma

  imm <- number
  return inst {rt = rt', immediate=imm}

completePart entries inst@(Instruction {instType = I, opCode=LwOpc}) = do
  (rt', imm, rs') <- completeLwSw entries
  return inst {rt = rt', immediate = imm, rs = rs'}

completePart entries inst@(Instruction {instType = I, opCode=SwOpc}) = do
  (rt', imm, rs') <- completeLwSw entries
  return inst {rt = rt', immediate = imm, rs = rs'}

completePart entries inst@(Instruction {instType = I}) = do
  (rs', rt', jl) <- completeBranch (rData entries) (jlabels entries)
  return inst {rt = rt', rs = rs', address = jl}

completePart entries inst@(Instruction {instType = J}) = do
  l <- identifier
  let mPc = lookup l (jlabels entries)
  case mPc of
    Nothing -> fail $ "Undefined label: " ++ l
    Just pc -> return inst {address = pc}

completeImmediate :: [DataEntry] -> Parser (Int, Int, Int)
completeImmediate entries = do
  r1 <- register entries
  comma

  r2 <- register entries
  comma

  imm <- number
  return (r1, r2, imm)

completeLwSw :: Globals -> Parser (Int, Int, Int)
completeLwSw entries = do
  r1 <- register $ rData entries
  comma

  imm <- memAddress $ mData entries
  comma

  r2 <- register $ rData entries

  return (r1, imm, r2)

completeBranch :: [DataEntry] -> [(String, Int)] -> Parser (Int, Int, Int)
completeBranch entries ls = do
  r1 <- register entries
  comma

  r2 <- register entries
  comma

  label' <- identifier
  let mPc = lookup label' ls
  case mPc of
    Nothing -> fail $ "Undefined label: " ++ label'
    Just pc -> return (r1, r2, pc)

shifts :: [DataEntry] -> Instruction -> Parser Instruction
shifts entries inst = do
  (rd' ,rt', shamt') <- completeImmediate entries
  return inst{rd = rd', rt = rt', shamt=shamt'}

mData :: Globals -> [DataEntry]
mData (x, _, _) = x

rData :: Globals -> [DataEntry]
rData (_, x, _) = x

jlabels :: Globals -> [(String, Int)]
jlabels (_, _, x) = x


