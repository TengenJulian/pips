import FRP.Yampa

import Data.IORef
import Data.List
import Data.Maybe
import Data.Either
import Data.Default
import Data.Foldable (toList)

import Debug.Trace

import Control.Lens
import Control.Monad
import Control.DeepSeq
import Control.Concurrent (newChan, writeChan, readChan, threadDelay, forkIO)

import Data.Sequence (Seq)
import qualified Data.Sequence as S

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
      matchAlias dat entries = foldl (\acc (loc, al) -> acc & ix loc . _1 .~ al) (map (\x -> ("", x)) (toList dat)) alias
        where alias = [(loc, al) | DataEntry loc (Just al) _ <- entries]

  args <- getArgs
  when ("--debug" `elem` args) $ do
    reactimate (return True) (senseInput True) actuateDebug archSF
    exitSuccess

  chanToUi    <- newChan
  chanToYampa <- newChan
  doneRef     <- newIORef False
  resultRef   <- newIORef arch

  handle' <- reactInit (return False) (const $ actuate doneRef resultRef) archSF
  handleRef <- newIORef handle'

  let benchmakCycles = do
        i      <- elemIndex "--benchmark" args
        cycles <- args ^? ix (i + 1)
        cycles ^? decimal
  when (has _Just benchmakCycles) $ do
    let Just cycles = benchmakCycles

    sec <- benchIOAction $
      forM_ [2..cycles] $
        return $ react handle' (10, Nothing)

    putStrLn $ unwords ["Simulating for", show cycles, "cycles took", show sec]
    readIORef resultRef >>= print
    exitSuccess

  ref <- readIORef resultRef
  writeChan chanToUi (ChangeEvent 1 ref)

  forkIO . forever $ do
    msg    <- readChan chanToYampa
    handle <- readIORef handleRef

    case msg of
      QuitMessage -> do
        writeIORef doneRef True
        react handle (0, Nothing)
        return ()

      CyclesMessage c -> do
        forM_ [1..c] $ \_ -> do
            react handle (10, Nothing)
            react handle (10, Nothing)
        archComp' <- readIORef resultRef

        when (c > 1) $ do
          let regPairs = matchAlias (dRegister archComp') reg
              memPairs = matchAlias (dMemory   archComp') mem

          writeChan chanToUi (LoadMemory   memPairs)
          writeChan chanToUi (LoadRegistry regPairs)

        writeChan chanToUi (ChangeEvent c archComp')

      RestartMessage -> do
        writeIORef doneRef True
        react handle (0, Nothing)
        writeIORef doneRef False

        writeChan chanToUi (ReloadEvent source (matchAlias (dRegister arch) reg) (matchAlias (dMemory   arch) mem))
        newHandle <- reactInit (return False) (const $ actuate doneRef resultRef) archSF
        writeIORef handleRef newHandle

        archComp' <- readIORef resultRef
        writeChan chanToUi (ChangeEvent 1 archComp')

  void $ M.customMain (V.mkVty def) chanToUi theApp
       $ initialState chanToYampa source (matchAlias (dRegister arch) reg)
       $ matchAlias (dMemory   arch) mem

senseInput :: Bool -> Bool -> IO (DTime, Maybe Bool)
senseInput debug _
  | False        = return (0.0, Just debug)
  | otherwise    = do
    input <- getLine
    putStrLn $ "input: " ++ input
    return (10.0, Just debug)

actuate :: Show a => IORef Bool -> IORef a -> Bool -> a -> IO Bool
actuate doneRef resultRef _ output = do
  done <- readIORef doneRef

  writeIORef resultRef output
  return done

actuateS :: IORef Bool -> Bool -> String -> IO Bool
actuateS doneRef _ output = do
  done <- readIORef doneRef
  putStrLn output
  return done

actuateDebug :: Bool -> ArchitectureComp -> IO Bool
actuateDebug _ archComp = do
  putStrLn $ dDebug archComp
  return False

-- | Returns the number of seconds it took the action to complete.
benchIOAction :: NFData b => IO b -> IO Double
benchIOAction a = do
  start <- getCPUTime
  r <- a
  end <- r `deepseq` getCPUTime

  return $ fromIntegral (end - start) / (10^12)
