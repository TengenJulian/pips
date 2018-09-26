import FRP.Yampa

import Data.Char (digitToInt, isDigit)
import Data.IORef
import Data.List
import Data.Either
import Data.Foldable (toList)

import Lens.Micro

import Control.Monad
import Control.DeepSeq
import Control.Concurrent (forkIO)

import Text.Parsec (parse)

import Brick.BChan
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)

import System.Exit
import System.CPUTime
import System.Environment

import Pips.Parser (parseFile)
import Pips.Assembler
import Pips.Architecture

import TUI

senseInput :: Bool -> Bool -> IO (DTime, Maybe Bool)
senseInput debug _
  | False        = return (0.0, Just debug)
  | otherwise    = do
    input <- getLine
    putStrLn $ "input: " ++ input
    return (10.0, Just debug)

actuate :: IORef Bool -> IORef a -> Bool -> a -> IO Bool
actuate doneRef resultRef _ output = do
  done <- readIORef doneRef

  writeIORef resultRef output
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

run :: String -> Bool -> Maybe Integer -> IO ()
run sourceFile debugMode benchmarkCycles = do

  s <- readFile sourceFile
  let source = s ++ "\n "
  let res    = assemble source

  when (isLeft res) $ do
    let Left err = res
    putStrLn err
    exitFailure

  let Right (Assembled reg mem insts endLabelMap) = res

  let arch = init16x16 reg mem insts
      archSF = architecture arch endLabelMap

      matchAlias dat entries = foldl (\acc (loc, al) -> acc & ix loc . _1 .~ al)
                               (map (\x -> ("", x)) (toList dat)) alias
        where alias = [(loc, al) | DataEntry loc (Just al) _ <- entries]

  when debugMode $ do
    reactimate (return True) (senseInput True) actuateDebug archSF
    exitSuccess

  chanToUi     <- newBChan 16
  chanToYampa' <- newBChan 16
  doneRef      <- newIORef False
  resultRef    <- newIORef arch

  handle' <- reactInit (return False) (const $ actuate doneRef resultRef) archSF
  handleRef <- newIORef handle'

  when (has _Just benchmarkCycles) $ do
    let Just cycles = benchmarkCycles

    sec <- benchIOAction $
      forM_ [2..cycles] $
        return $ react handle' (10, Nothing)

    putStrLn $ unwords ["Simulating for", show cycles, "cycles took", show sec]
    readIORef resultRef >>= print
    exitSuccess

  ref <- readIORef resultRef
  writeBChan chanToUi (ChangeEvent 1 ref)

  forkIO . forever $ do
    msg    <- readBChan chanToYampa'
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

          writeBChan chanToUi (LoadMemory   memPairs)
          writeBChan chanToUi (LoadRegistry regPairs)

        writeBChan chanToUi (ChangeEvent c archComp')

      RestartMessage -> do
        writeIORef doneRef True
        react handle (0, Nothing)
        writeIORef doneRef False

        writeBChan chanToUi (ReloadEvent source (matchAlias (dRegister arch) reg) (matchAlias (dMemory   arch) mem))
        newHandle <- reactInit (return False) (const $ actuate doneRef resultRef) archSF
        writeIORef handleRef newHandle

        archComp' <- readIORef resultRef
        writeBChan chanToUi (ChangeEvent 1 archComp')

  putStrLn "Starting simulation"
  let cfg = (V.mkVty defaultConfig)
  void $ M.customMain cfg (Just chanToUi) theApp
       $ initialState chanToYampa' source (matchAlias (dRegister arch) reg)
       $ matchAlias (dMemory arch) mem

main :: IO ()
main = do
  args <- getArgs

  let sourceFiles = filter (not . isPrefixOf "--") args

  when (null sourceFiles) $ do
    putStrLn "No source file specified."
    exitFailure

  let sourceFile = head sourceFiles
  putStrLn $ "source file: " ++ sourceFile

  when ("--parser-output" `elem` args) $ do
    source <- readFile sourceFile

    case parse parseFile sourceFile source of
      Left err -> print err >> exitFailure
      Right (rd, md, tokens) -> do
        putStrLn "Reg data:"
        mapM_ print rd

        putStrLn "\nMemory data:"
        mapM_ print md

        putStrLn "\nTokens:"
        mapM_ print tokens

        exitSuccess

  when ("--assembler-output" `elem` args) $ do
    source <- readFile sourceFile

    case assemble source of
      Left err -> print err >> exitFailure
      Right (Assembled rd md ints _) -> do
        putStrLn "Reg data:"
        mapM_ print rd

        putStrLn "\nMemory data:"
        mapM_ print md

        putStrLn "\nInstructions:"
        mapM_ print ints

        exitSuccess

  let benchmarkCycles = do
        i      <- elemIndex "--benchmark" args
        cycles <- args ^? ix (i + 1)
        let toDigit c
              | isDigit c  = Just . fromIntegral $ digitToInt c
              | otherwise = Nothing

        ds <- mapM toDigit cycles

        return (foldl (\acc c -> acc * 10 + c) 0 ds)

  run sourceFile ("--debug" `elem` args) benchmarkCycles
