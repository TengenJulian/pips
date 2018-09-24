module Main where

import           Brick.BChan
import qualified Brick.Main as M

import           Control.Concurrent (forkIO)
import           Control.DeepSeq
import           Control.Monad

import           Data.Foldable (toList)
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Semigroup ((<>))

import           FRP.Yampa hiding (switch)

import qualified Graphics.Vty as V
import           Graphics.Vty.Config (defaultConfig)

import           Options.Applicative

import           System.Directory
import           System.CPUTime
import           System.Exit

import           Text.Parsec (parse)

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
  r     <- a
  end   <- r `deepseq` getCPUTime

  return $ fromIntegral (end - start) / (10^(12 :: Int))

runDebug :: SF Bool ArchitectureComp -> IO ()
runDebug archSF = do
    reactimate (return True) (senseInput True) actuateDebug archSF
    exitSuccess

runWithoutTui :: ArchitectureComp -> SF Bool ArchitectureComp -> Int -> IO ()
runWithoutTui arch archSF numCycles = do
  doneRef   <- newIORef False
  resultRef <- newIORef arch
  handle'   <- reactInit (return False) (const $ actuate doneRef resultRef) archSF

  secs <- benchIOAction $
    forM_ [2..numCycles] $
      return $ react handle' (10, Nothing)

  putStrLn $ unwords ["Simulating for", show numCycles, "cycles took", show secs, "seconds"]
  readIORef resultRef >>= print
  exitSuccess

run :: String -> IO ()
run source' = do
  Assembled reg mem insts endLabelMap <- runEither $ assemble source'

  let arch = init16x16 reg mem insts
      archSF = architecture arch endLabelMap

      matchAlias entries dat = zipWith (\i d -> (M.findWithDefault "" i aliases, d)) [0..] (toList dat)
        where aliases = M.fromList [(loc, al) | DataEntry loc (Just al) _ <- entries]

      matchAliasMem = matchAlias mem
      matchAliasReg = matchAlias reg

  chanToUi     <- newBChan 16
  chanToYampa' <- newBChan 16
  doneRef      <- newIORef False
  resultRef    <- newIORef arch

  handle' <- reactInit (return False) (const $ actuate doneRef resultRef) archSF
  handleRef <- newIORef handle'

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
          let regPairs = matchAliasReg $ dRegister archComp'
              memPairs = matchAliasMem $ dMemory   archComp'

          writeBChan chanToUi (LoadMemory   memPairs)
          writeBChan chanToUi (LoadRegistry regPairs)

        writeBChan chanToUi (ChangeEvent c archComp')

      RestartMessage -> do
        writeIORef doneRef True
        react handle (0, Nothing)
        writeIORef doneRef False

        writeBChan chanToUi (ReloadEvent source' (matchAliasReg $ dRegister arch) (matchAliasMem $ dMemory arch))
        newHandle <- reactInit (return False) (const $ actuate doneRef resultRef) archSF
        writeIORef handleRef newHandle

        archComp' <- readIORef resultRef
        writeBChan chanToUi (ChangeEvent 1 archComp')

  putStrLn "Starting simulation"

  let cfg = V.mkVty defaultConfig
  void $ M.customMain cfg (Just chanToUi) theApp
       $ initialState chanToYampa' source'
           (matchAliasReg $ dRegister arch)
           (matchAliasMem $ dMemory arch)

data ProgMode =
  ProgSim
  | ProgSimDebug
  | ProgSimWithTui Int
  | ProgAssemblerOutput
  | ProgParserOutput
  deriving (Eq, Show)

data CmdArgs = CmdArgs
  { argsSource :: FilePath
  , argsProgMod :: ProgMode
  }
  deriving (Eq, Show)

cmdArgsProgMode :: Parser ProgMode
cmdArgsProgMode =
      pure ProgSim
  <|> ProgSimWithTui
        <$> option auto
          (    long "no-tui"
            <> help "Runs the simulator for N cycles, without the tui"
            <> metavar "N" )
  <|> const ProgSimDebug
        <$> switch
          (    long "debug"
            <> help "Run the simulator in debug mode" )
  <|> const ProgAssemblerOutput
        <$> switch
          (    long "assembler-output"
            <> help "Output the assembler's output, and stop the program" )
  <|> const ProgParserOutput
        <$> switch
          (    long "parser-output"
            <> help "Output the parser's output, and stop the program" )

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
  <$> argument str
    (    metavar "SOURCE_FILE"
      <> help "Assembly file to run" )
  <*> cmdArgsProgMode

runEither :: Either String a -> IO a
runEither (Left e)  = putStrLn e >> exitFailure
runEither (Right x) = return x

runEitherShow :: Show e => Either e a -> IO a
runEitherShow (Left e)  = runEither (Left $ show e)
runEitherShow (Right x) = runEither (Right x)

main :: IO ()
main = do
  let opts = info (cmdArgs <**> helper)
        (    fullDesc
          <> progDesc "Simulate SOURCE_FILE"
          <> header "pips - a simulator for a poor man's MIPS architecture" )

  args <- execParser opts

  let sourceFile = argsSource args

  b <- doesFileExist sourceFile
  unless b $ do
    putStrLn $ "File does not exist: " ++ sourceFile
    exitFailure

  putStrLn $ "source file: " ++ sourceFile

  s <- readFile sourceFile
  let source' = s ++ "\n"

  case argsProgMod args of
    ProgSim ->
      run source'

    ProgParserOutput -> do
      (rd, md, tokens ) <- runEitherShow $ parse parseFile sourceFile source'
      putStrLn "Reg data:"
      mapM_ print rd

      putStrLn "\nMemory data:"
      mapM_ print md

      putStrLn "\nTokens:"
      mapM_ print tokens

      exitSuccess

    ProgAssemblerOutput -> do
        Assembled rd md ints _ <- runEither $ assemble source'
        putStrLn "Reg data:"
        mapM_ print rd

        putStrLn "\nMemory data:"
        mapM_ print md

        putStrLn "\nInstructions:"
        mapM_ print ints

        exitSuccess

    mode -> do
      Assembled reg mem insts endLabelMap <- runEither $ assemble source'
      let arch = init16x16 reg mem insts
          archSF = architecture arch endLabelMap

      case mode of
        ProgSimWithTui n ->
          runWithoutTui arch archSF n
        _               ->
          runDebug archSF
