{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module TUI
  ( module TUI
  , module TUI.Table
  ) where

import Pips.Architecture (ArchitectureComp, dLineNum, dMemory, dRegister, dRegisterChange, dMemoryChange)

import TUI.Table

import Numeric.Lens
import Control.Lens
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Concurrent (Chan, writeChan, readChan)

import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Data.Monoid
import Data.Data.Lens()
import Data.Vector.Lens()
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T (null)

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Markup (markup, (@?))
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , str
  , vBox
  , hBox
  , hLimit
  , withAttr
  , padRight
  , emptyWidget
  )
import Brick.Util (fg, on)

data St =
    St {
         _memory       :: Table String TableCell
       , _register     :: Table String TableCell
       , _source       :: Table String TableCell
       , _widgetFocus  :: Focus
       , _numberFormat :: NumberFormat
       , _clockCycles  :: Int
       , _appMode      :: AppMode
       , _chanToYampa  :: Chan YampaMessage
       , _cycleEditor  :: E.Editor
       , _cycleStepSize :: Int
       , _errorMsg     :: Text
       }

data AppEvent = VtyEvent V.Event
              | ChangeEvent Int ArchitectureComp
              | DataChangeEvent [DataChange] Int
              | SetDataEvent [Int] [Int]
              | ReloadEvent String [(String, Int)] [(String, Int)]
              | LoadMemory [(String, Int)]
              | LoadRegistry [(String, Int)]
              deriving (Show, Eq)

data DataChange = MemoryChange Int Int | RegisterChange Int Int deriving (Show, Eq)

data AppMode = NormalMode | CycleMode deriving (Show, Eq)

data YampaMessage = CyclesMessage Int
                  | QuitMessage
                  | RestartMessage
                   deriving (Show, Eq)

data NumberFormat = DecimalFormat  | HexFormat   | BinaryFormat deriving (Show, Eq, Enum, Bounded)
data Focus = MemTable | RegTable | SourceCode deriving (Eq, Show, Enum, Bounded)

data TableCell = StringCell String | IntCell Int | ValueCell Int deriving (Show, Eq)

makeLenses ''St

drawUI :: St -> [Widget]
drawUI st = case st ^. appMode of
  NormalMode -> [ui]
  CycleMode  -> [C.center editBox]

  where editor = st ^. cycleEditor
        errorWidget
          | T.null (st ^. errorMsg) = emptyWidget
          | otherwise               = markup ((st ^. errorMsg) @? errorAttr)

        editBox = B.borderWithLabel (str " Cycles per Step ")  . hLimit 50 $
          C.hCenter errorWidget
          <=> E.renderEditor editor
          <=> emptyWidget

        highlightFocus True  = withAttr focusAttr
        highlightFocus False = id
        focus = st ^. widgetFocus

        memBox    = B.borderWithLabel (highlightFocus (focus == MemTable) (str " Memory ")) $
          renderTable (st ^. memory) drawLeftAligned (drawCell (st ^. numberFormat))

        regBox    = B.borderWithLabel (highlightFocus (focus == RegTable) (str " Register ")) $
          renderTable (st ^. register) drawLeftAligned (drawCell (st ^. numberFormat))

        sourceBox = B.borderWithLabel (highlightFocus (focus == SourceCode) (str " Source ")) $
          renderTable (st ^. source) drawLeftAligned (drawCell (st ^. numberFormat))

        ui = C.vCenter $ vBox [ C.hCenter $ hBox [memBox, regBox]
                              , C.hCenter sourceBox
                              , str " "
                              , C.hCenter . str $  "Clock Cycle: " <> show (st ^. clockCycles)
                                                <> " Step Size: "  <> show (st ^. cycleStepSize)
                              , C.hCenter $ str "[r] Reload [q/Esq] Quit [f] Change number format"
                              , C.hCenter $ str "[n] Advance Clock Cycle(s) [N] Change Clock Cycle per step"
                              , C.hCenter $ str "[up/down] Scroll View [tab] Switch View"
                              ]

drawLeftAligned :: String -> Widget
drawLeftAligned = padRight T.Max . str

formatter :: NumberFormat -> Int -> String
formatter DecimalFormat = show
formatter HexFormat     = ("0x" ++) . (^. re hex)
formatter BinaryFormat  = ("0b" ++) . (^. re binary)

drawCell :: NumberFormat -> RowMode -> TableCell -> Widget
drawCell format Highlighted cell  = withAttr highlightAttr $ drawCell format Normal cell
drawCell _      _ (StringCell s)  = drawLeftAligned $ "  " ++ s
drawCell _      _ (IntCell n)     = drawLeftAligned $ "  " ++ show n
drawCell format _ (ValueCell n)   = drawLeftAligned $ "  " ++ formatter format n

getFocus :: St -> Lens' St (Table String TableCell)
getFocus St {_widgetFocus = MemTable} = memory
getFocus St {_widgetFocus = RegTable} = register
getFocus St {_widgetFocus = SourceCode} = source

cycleEnum :: (Enum a, Bounded a, Eq a) => a -> a
cycleEnum x = if maxBound == x then minBound else succ x

appEvent :: St -> AppEvent -> T.EventM (T.Next St)
appEvent st@(St {_appMode = CycleMode}) (VtyEvent ev) = cycleInputEvent st ev

appEvent st (LoadMemory mem) = do
  let (names, vals) = unzip mem
  M.continue $ st & memory . tableCellT 1 .~ map StringCell names
                  & memory . tableCellT 2 .~ map ValueCell vals

appEvent st (LoadRegistry reg) = do
  let (names, vals) = unzip reg
  M.continue $ st & register . tableCellT 1 .~ map StringCell names
                  & register . tableCellT 2 .~ map ValueCell vals

appEvent st (ReloadEvent src reg mem) = M.continue $ initialState (st ^. chanToYampa) src reg mem

appEvent st (ChangeEvent cycles archComp) =
  let updateData _   Nothing  = id
      updateData dat (Just r) = (& tableCellT 2 . ix r .~ ValueCell (S.index dat r)) . flip tableSetHighlight [r]
  in  M.continue $ st & clockCycles +~ cycles
                      & memory   %~ updateData  (dMemory archComp)   (dMemoryChange archComp)
                      & source   %~ tableMoveTo (dLineNum archComp)
                      & register %~ updateData  (dRegister archComp) (dRegisterChange archComp)

appEvent st (VtyEvent e) = vtyEvent st e
-- appEvent st _            = M.continue st

vtyEvent :: St -> V.Event -> T.EventM (T.Next St)
vtyEvent st e =
    case e of
        V.EvKey (V.KChar 'q') [] -> sendYampaMsg st QuitMessage >> M.halt st
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & widgetFocus  %~ cycleEnum
        V.EvKey (V.KChar 'f')  [] -> M.continue $ st & numberFormat %~ cycleEnum
        V.EvKey (V.KChar 'N')  [] -> M.continue $ st & appMode .~ CycleMode
        V.EvKey (V.KChar 'n')  [] -> do
            sendYampaMsg st . CyclesMessage $ st ^. cycleStepSize
            M.continue st

        V.EvKey (V.KChar 'r') []  -> do
          let newSt = st & clockCycles .~ 0
          sendYampaMsg st RestartMessage
          M.continue newSt

        ev -> traverseOf (getFocus st) (T.handleEvent ev) st >>= M.continue

cycleInputEvent :: St -> V.Event -> T.EventM (T.Next St)
cycleInputEvent st (V.EvKey V.KEsc []) = M.continue (st & appMode .~ NormalMode)
cycleInputEvent st (V.EvKey V.KEnter []) = do
  let editorText = unwords . E.getEditContents $ st ^. cycleEditor
  case editorText ^? decimal of
    Nothing       -> M.continue $ st & errorMsg .~ "Step size must be a number"
    Just x
      | x < 0     -> M.continue $ st & errorMsg      .~ "Step size must be a positive number"

      | otherwise -> M.continue $ st & cycleStepSize .~ x
                                     & appMode       .~ NormalMode
                                     & errorMsg      .~ ""

cycleInputEvent st ev = M.continue =<< T.handleEventLensed st cycleEditor ev

sendYampaMsg :: MonadIO m => St -> YampaMessage -> m ()
sendYampaMsg st = liftIO . writeChan (st ^. chanToYampa)

listDrawElement :: (Show a) => Bool -> a -> Widget
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> selStr (show a)

initialState :: Chan YampaMessage -> String -> [(String, Int)] -> [(String, Int)] -> St
initialState chan src reg mem =
  let src' = lines src
      regTable = makeTable (T.Name "registerTable") 1 [
                ("#"    , 6,  V.fromList . map IntCell    $ take (length reg) [0..]),
                ("Alias", 10, V.fromList . map StringCell $ map fst reg),
                ("Value", 40, V.fromList . map ValueCell  $ map snd reg)
               ]
      memTable = makeTable (T.Name "memoryTable") 1 [
                ("#"    , 6,  V.fromList . map IntCell    $ take (length mem) [0..]),
                ("Alias", 10, V.fromList . map StringCell $ map fst mem),
                ("Value", 40, V.fromList . map ValueCell  $ map snd mem)
               ]
      sourceTable = makeTable (T.Name "sourceTable") 1 [
                ("#"    , 6,  V.fromList . map IntCell    $ take (length src') [1..]),
                (" ", 10000,  V.fromList . map StringCell $ src')
               ]

      editor = E.editor "cycleEditor" (str . unlines) (Just 1) "1"
  in St {
    _memory = memTable
    , _register = regTable
    , _source = sourceTable
    , _widgetFocus = SourceCode
    , _numberFormat = DecimalFormat
    , _clockCycles = 0
    , _appMode = NormalMode
    , _chanToYampa = chan
    , _cycleEditor = editor
    , _cycleStepSize = 1
    , _errorMsg = ""
    }

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

highlightAttr :: A.AttrName
highlightAttr = "highlight"

errorAttr :: A.AttrName
errorAttr = "error"

focusAttr :: A.AttrName
focusAttr = "focus"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr           , V.white `on` V.blue)
    , (L.listSelectedAttr   , V.blue  `on` V.white)
    , (customAttr           , fg V.cyan)
    , (highlightAttr        , V.white `on` V.green)
    , (errorAttr            , V.white `on` V.red)
    , (focusAttr            , V.white  `on` V.blue)
    ]

theApp :: M.App St AppEvent
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = VtyEvent
          }
