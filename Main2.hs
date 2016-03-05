import FRP.Yampa

import Data.IORef
import Data.List
import Data.Maybe
import Data.Either
import Data.Default

import Debug.Trace

import Control.Lens
import Control.Monad
import Control.DeepSeq
import Control.Concurrent (newChan, writeChan, readChan, threadDelay, forkIO)

import Pips.Assembler
import Pips.Components
import Pips.Instruction
import Pips.Architecture

import Numeric.Lens

import Text.Parsec
import Text.Parsec.String

import qualified Brick.Main as M
import qualified Graphics.Vty as V

import TUI

import System.Exit
import System.CPUTime
import System.Environment

main = do
  source <- readFile "examples/fib.wasm"
  let res  = assemble source
      Left err                = res

  when (isLeft res) $ do
    putStrLn err
    exitFailure
  let Right (reg, mem, insts) = res
  print reg
  print mem

  --putStrLn "Starting...\n-----------------------"
  let arch = init16x16 reg mem insts
      archSF = architecture arch
      matchAlias dat entries = foldl (\acc (loc, al) -> acc & ix loc . _1 .~ al) (map (\x -> ("", x)) dat) alias
        where alias = [(loc, al) | DataEntry loc (Just al) _ <- entries]

  let dt = deltaEncode 20 (replicate 100 True)
  return ()
