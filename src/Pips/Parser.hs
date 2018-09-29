module Pips.Parser
  ( module Pips.Parser
  ) where

import Data.Functor.Identity
import Data.Maybe

import Text.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok

import Pips.Common
import Pips.Instruction ()

data InstructionName =
  AddN
  | SubN
  | AndN
  | OrN
  | XorN
  | SltN
  | SllN
  | SrlN
  | MulN
  | AddiN
  | LuiN
  | JrN
  | JN
  | BeqN
  | BneN
  | SwN
  | LwN
  deriving (Show, Eq)

data RegName = RegAlias String     | RegNum UInt deriving (Show, Eq)
data Label   = LabelName String    | LabelNum Int deriving (Show, Eq)
data MemAddr = MemAddrAlias String | MemAddrNum Int deriving (Show, Eq)

data DataEntry n = DataEntry Int (Maybe String) n deriving (Show, Eq)

data Token =
  Reg3Token Int InstructionName RegName RegName RegName
  -- Sub Int RegName RegName RegName
  -- And Int RegName RegName RegName
  -- Xor Int RegName RegName RegName
  -- Slt Int RegName RegName RegName

  | Reg2iToken Int InstructionName RegName RegName Int
  -- Addi Int RegName RegName Int
  -- Sll Int RegName RegName Int
  -- Srl Int RegName RegName Int

  | LuiToken Int RegName Int

  | JrToken Int RegName
  | JToken Int Label

  | BranchToken Int InstructionName RegName RegName Label
  -- Beq Int RegName RegName Label
  -- Bne Int RegName RegName Label

  | MemOpToken Int InstructionName RegName MemAddr RegName
  -- Sw Int RegName MemAddr RegName
  -- Lw Int RegName MemAddr RegName

  | LabelToken Int String
  | CommentToken Int String
  deriving (Show, Eq)

tokenLineNum :: Token -> Int
tokenLineNum (Reg3Token     l _ _ _ _) = l
tokenLineNum (Reg2iToken    l _ _ _ _) = l
tokenLineNum (LuiToken      l _ _)     = l
tokenLineNum (JrToken       l _)       = l
tokenLineNum (JToken        l _)       = l
tokenLineNum (BranchToken   l _ _ _ _) = l
tokenLineNum (MemOpToken    l _ _ _ _) = l
tokenLineNum (LabelToken    l _)       = l
tokenLineNum (CommentToken  l _)       = l

opNames :: [String]
opNames = [
      "add"
      , "sub"
      , "and"
      , "or"
      , "sll"
      , "srl"
      , "slt"
      , "jr"
      , "addi"
      , "lui"
      , "lw"
      , "sw"
      , "beq"
      , "bme"
      , "j"
      ]

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser haskellDef {
  Tok.reservedNames = opNames
  , Tok.caseSensitive = False
  , Tok.nestedComments = False
  , Tok.commentLine = "#"
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum
  }

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer
commaSep1 = Tok.commaSep1 lexer

comma, colon :: Parser String
comma = Tok.comma lexer
colon = Tok.colon lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

identifier :: Parser String
identifier = Tok.identifier lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

octal, decimal, hexadecimal :: Parser Integer
octal = Tok.octal lexer
decimal = Tok.decimal lexer
hexadecimal = Tok.hexadecimal lexer

binary :: Parser Int
binary = do
  try (string "0b")
  s <- many1 (char '0' <|> char '1')
  return $ foldl (\acc d -> 2 * acc + read [d]) 0 s

natural, integer :: Parser Integer
natural = Tok.natural lexer
integer = Tok.integer lexer

number, posNumber :: Parser Int
number    = binary <|> (fromIntegral <$> integer)
posNumber = binary <|> (fromIntegral <$> natural)

comment :: Parser Token
comment = do
  l <- lineNum'
  c <- char '#'
  rest <- many (noneOf "\n\r")

  return $ CommentToken l (c : rest)

lineNum' :: Parser Int
lineNum' = fmap ((+(-1)) . sourceLine) getPosition

parseRegName :: Parser RegName
parseRegName = char '$' >>
  ((RegAlias <$> identifier) <|> (RegNum . fromIntegral <$> natural))

parseLabel :: Parser Token
parseLabel = do
  ln <- lineNum'
  label' <- identifier

  char ':'
  whiteSpace

  return (LabelToken ln label')

labelRef :: Parser Label
labelRef = LabelName <$> identifier

parseMemAddr :: Parser MemAddr
parseMemAddr = (MemAddrAlias <$> identifier) <|> (MemAddrNum . fromIntegral <$> number)

regs3 :: InstructionName -> String -> Parser Token
regs3 inn opName = Reg3Token
      <$> (reserved opName >> lineNum')
      <*> pure inn
      <*> parseRegName
      <*> (comma >> parseRegName)
      <*> (comma >> parseRegName)

regs2i :: InstructionName -> String -> Parser Token
regs2i inn opName = Reg2iToken
      <$> (reserved opName >> lineNum')
      <*> pure inn
      <*> parseRegName
      <*> (comma >> parseRegName)
      <*> (fromIntegral <$> (comma >> number))

branch :: InstructionName -> String -> Parser Token
branch inn opName = BranchToken
      <$> (reserved opName >> lineNum')
      <*> pure inn
      <*> parseRegName
      <*> (comma >> parseRegName)
      <*> (comma >> labelRef)

memOp :: InstructionName -> String -> Parser Token
memOp inn opName = MemOpToken
      <$> (reserved opName >> lineNum')
      <*> pure inn
      <*> parseRegName
      <*> (comma >> parseMemAddr)
      <*> (comma >> parseRegName)

lui, jr, jump :: Parser Token
lui = LuiToken
       <$> (reserved "lui" >> lineNum')
       <*> parseRegName
       <*> (fromIntegral <$> (comma >> number))

jr = JrToken
      <$> (reserved "jr" >> lineNum')
      <*> parseRegName

jump = JToken
      <$> (reserved "j" >> lineNum')
      <*> labelRef

parseToken :: Parser Token
parseToken = regs3 AddN "add"
  <|> regs3 SubN "sub"
  <|> regs3 AndN "and"
  <|> regs3 MulN "mult"
  <|> regs3 XorN "xor"
  <|> regs3 SltN "slt"
  <|> regs3  OrN "or"
  <|> regs2i SllN "sll"
  <|> regs2i SrlN "srl"
  <|> regs2i AddiN "addi"
  <|> lui
  <|> jr
  <|> jump
  <|> branch BeqN "beq"
  <|> branch BneN "bne"
  <|> memOp SwN "sw"
  <|> memOp LwN "lw"
  <|> comment
  <|> parseLabel

staticData :: Num n => Parser [DataEntry n]
staticData = single `endBy` whiteSpace
  where single = do
          location <- posNumber
          lexeme (char ':')
          lexeme (string "WORD")

          alias <- optionMaybe identifier
          value <- fromIntegral <$> number

          return $ DataEntry location alias value

parseRegData, parseMemData :: Num n => Parser [DataEntry n]
parseRegData = try (string ".register") >> whiteSpace >> staticData
parseMemData = try (string ".memory") >> whiteSpace >> staticData

parseFile :: Parser ([DataEntry Int], [DataEntry Int], [Token])
parseFile = do
  regData <- fromMaybe [] <$> optionMaybe parseRegData
  memData <- fromMaybe [] <$> optionMaybe parseMemData

  tokens' <- parseToken `endBy` whiteSpace

  eof

  return (regData, memData, tokens')
