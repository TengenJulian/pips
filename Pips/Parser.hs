module Parser where

import Pips.Instruction
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language (haskellDef)

import qualified Text.Parsec.Token as Tok

import Control.Lens
import Numeric.Lens

import Data.Int


data InstructionName = InstructionName deriving (Show, Eq)
data NameValue a = Name String | Value a deriving (Show, Eq)
data Token =
  IToken Int InstructionName (NameValue Int) (NameValue Int) (NameValue Int)
  | LabelToken Int String
  | CommentToken Int
  deriving (Show, Eq)

lexer = Tok.makeTokenParser haskellDef {
  Tok.reservedNames = [
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
  , Tok.caseSensitive = True
  , Tok.nestedComments = False
  , Tok.commentLine = "#"
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum
  }

commaSep = Tok.commaSep lexer
commaSep1 = Tok.commaSep1 lexer

instructionName :: Parser InstructionName
instructionName = undefined

number :: Parser Int
number = do
  prefix <- try (string "0x") <|> try (string "0b") <|> fmap (:[]) digit

  case prefix of
    "0x" -> fmap (^?! hex)     $ many1 hexDigit
    "0b" -> fmap (^?! binary)  $ many1 (oneOf "01")
    _    -> fmap (^?! decimal) $ many digit

indentifier :: Parser String
indentifier = do
  l <- letter
  rest <- many (letter <|> digit)

  return (l : rest)

comment :: Parser Token
comment = fmap CommentToken $ do
  c <- char '#'
  rest <- many (noneOf "\n\r")

  return (c : rest)
