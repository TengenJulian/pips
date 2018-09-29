module Pips.Instruction
  ( module Pips.Instruction
  ) where

import Pips.Common

data OpCode = ROpc | AddiOpc | LuiOpc | LwOpc | SwOpc | BeqOpc | BneOpc | JOpc deriving (Eq, Show)

opcodeToInt :: OpCode -> Int
opcodeToInt AddiOpc= 0x0c
opcodeToInt LuiOpc = 0x0f
opcodeToInt LwOpc  = 0x23
opcodeToInt SwOpc  = 0x2b
opcodeToInt BeqOpc = 0x04
opcodeToInt BneOpc = 0x05
opcodeToInt JOpc   = 0x02
opcodeToInt ROpc   = 0x00

intToOpCode :: Int -> OpCode
intToOpCode 0x0c = AddiOpc
intToOpCode 0x0f = LuiOpc
intToOpCode 0x23 = LwOpc
intToOpCode 0x2b = SwOpc
intToOpCode 0x04 = BeqOpc
intToOpCode 0x05 = BneOpc
intToOpCode 0x02 = JOpc
intToOpCode _    = ROpc

data Instruction = Instruction
  { opCode :: OpCode
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

aluOpToInt :: AluOp -> Int
aluOpToInt MulOp = 0x18
aluOpToInt AddOp = 0x20
aluOpToInt SubOp = 0x22
aluOpToInt AndOp = 0x24
aluOpToInt OrOp  = 0x25
aluOpToInt SllOp = 0x00
aluOpToInt SrlOp = 0x02
aluOpToInt SltOp = 0x2a
aluOpToInt JrOp  = 0x08
aluOpToInt XorOp = 0x26
aluOpToInt LuiOp = 0x50 -- non conform mips

intToAluOp :: Int -> AluOp
intToAluOp 0x18 = MulOp
intToAluOp 0x20 = AddOp
intToAluOp 0x22 = SubOp
intToAluOp 0x24 = AndOp
intToAluOp 0x25 = OrOp
intToAluOp 0x2a = SltOp
intToAluOp 0x08 = JrOp
intToAluOp 0x26 = XorOp
intToAluOp 0x50 = LuiOp -- non-conform mips
intToAluOp 0x02 = SrlOp
intToAluOp _    = SllOp

nop :: Instruction
nop = Instruction ROpc R 0 0 0 0 0 SllOp "" 0 0

isBranchInst :: Instruction -> Bool
isBranchInst inst = opCode inst `elem` [JOpc, BeqOpc, BneOpc] || aluOp inst == JrOp
