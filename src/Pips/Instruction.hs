module Pips.Instruction
  ( module Pips.Instruction
  ) where

import Pips.Common

data OpCode = ROpc | AddiOpc | LuiOpc | LwOpc | SwOpc | BeqOpc | BneOpc | JOpc deriving (Eq, Show)

instance Enum OpCode where
  toEnum 0x00 = ROpc
  toEnum 0x0c = AddiOpc
  toEnum 0x0f = LuiOpc
  toEnum 0x23 = LwOpc
  toEnum 0x2b = SwOpc
  toEnum 0x04 = BeqOpc
  toEnum 0x05 = BneOpc
  toEnum 0x02 = JOpc

  fromEnum ROpc   = 0x00
  fromEnum AddiOpc= 0x0c
  fromEnum LuiOpc = 0x0f
  fromEnum LwOpc  = 0x23
  fromEnum SwOpc  = 0x2b
  fromEnum BeqOpc = 0x04
  fromEnum BneOpc = 0x05
  fromEnum JOpc   = 0x02

data Instruction =
  Instruction {
    opCode :: OpCode
    , instType :: InstructionType
    , rs :: UInt
    , rt :: UInt
    , rd :: UInt
    , address :: UInt
    , shamt :: UInt
    , aluOp :: AluOp
    , original :: String
    , immediate :: UInt
    , lineNum :: Int
  } deriving (Eq, Show)

data InstructionType = R | I | J deriving (Show, Eq)

data AluOp = AddOp | SubOp | MulOp | AndOp | OrOp | SllOp | SrlOp | SltOp | JrOp | XorOp | LuiOp deriving (Show, Eq)

instance Enum AluOp where
  toEnum 0x18 = MulOp
  toEnum 0x20 = AddOp
  toEnum 0x22 = SubOp
  toEnum 0x24 = AndOp
  toEnum 0x25 = OrOp
  toEnum 0x00 = SllOp
  toEnum 0x02 = SrlOp
  toEnum 0x2a = SltOp
  toEnum 0x08 = JrOp
  toEnum 0x26 = XorOp

  -- non conform mips
  toEnum 0x50 = LuiOp
  toEnum _    = SllOp

  fromEnum MulOp = 0x18
  fromEnum AddOp = 0x20
  fromEnum SubOp = 0x22
  fromEnum AndOp = 0x24
  fromEnum OrOp  = 0x25
  fromEnum SllOp = 0x00
  fromEnum SrlOp = 0x02
  fromEnum SltOp = 0x2a
  fromEnum JrOp  = 0x08
  fromEnum XorOp = 0x26

  -- non conform mips
  fromEnum LuiOp = 0x50

nop :: Instruction
nop = Instruction ROpc R 0 0 0 0 0 AddOp "" 0 0

isBranchInst :: Instruction -> Bool
isBranchInst inst = opCode inst `elem` [JOpc, BeqOpc, BneOpc] || aluOp inst == JrOp
