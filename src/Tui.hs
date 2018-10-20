{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Tui
  ( module Tui
  , module Tui.Table
  ) where

import qualified Brick.AttrMap as A
import           Brick.BChan
import qualified Brick.Main as M
import           Brick.Markup ( markup, (@?) )
import           Brick.Types ( Widget )
import qualified Brick.Types as T
import           Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core ( (<=>), str, vBox, hBox, hLimit,
                                      withAttr, padRight, emptyWidget )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L

import           Control.Monad.IO.Class ( liftIO, MonadIO )

import           Data.Char ( chr, intToDigit, ord, isDigit )
import qualified Data.Function as F ( on )
import           Data.Monoid
import           Data.Sequence ()
import qualified Data.Sequence as S
import           Data.Text ( Text )
import qualified Data.Text as T ( null )
import qualified Data.Vector as V

import qualified Graphics.Vty as V

import           Lens.Micro
import           Lens.Micro.TH

import Pips.Architecture ( ArchComp, archLineNum, archMem, archReg,
                           archRegChange, archMemChange )
import Pips.Common

import Tui.Table

data WidgetId =
  MemTableId
  | MemColId Int
  | RegTableId
  | RegColId Int
  | SourceTableId
  | SourceColId Int
  | EditorId
  | EditorColId Int
  | CycleEditorId
  deriving (Show, Eq)

widgetIdToInt :: WidgetId -> (Int, Int)
widgetIdToInt MemTableId      = (0, 0)
widgetIdToInt RegTableId      = (1, 0)
widgetIdToInt SourceTableId   = (2, 0)
widgetIdToInt EditorId        = (3, 0)
widgetIdToInt CycleEditorId   = (4, 0)
widgetIdToInt (MemColId n)    = (0, n)
widgetIdToInt (RegColId n)    = (1, n)
widgetIdToInt (SourceColId n) = (2, n)
widgetIdToInt (EditorColId n) = (3, n)

instance Ord WidgetId where
  compare = compare `F.on` widgetIdToInt

data St = St
  { _memory       :: Table WidgetId String TableCell
  , _register     :: Table WidgetId String TableCell
  , _source       :: Table WidgetId String TableCell
  , _widgetFocus  :: Focus
  , _numberFormat :: NumberFormat
  , _clockCycles  :: Int
  , _appMode      :: AppMode
  , _chanToYampa  :: BChan YampaMessage
  , _cycleEditor  :: E.Editor String WidgetId
  , _cycleStepSize :: Int
  , _errorMsg     :: Text
  }

data AppEvent =
  VtyEvent V.Event
  | ChangeEvent Int ArchComp
  | DataChangeEvent [DataChange] Int
  | SetDataEvent [Int] [Int]
  | ReloadEvent String [(String, UInt)] [(String, UInt)]
  | LoadMemory [(String, UInt)]
  | LoadRegistry [(String, UInt)]
  deriving (Show, Eq)

data DataChange = MemoryChange Int UInt | RegisterChange Int UInt deriving (Show, Eq)

data AppMode = NormalMode | CycleMode deriving (Show, Eq)

data YampaMessage = CyclesMessage Int | QuitMessage | RestartMessage deriving (Show, Eq)

data NumberFormat = DecimalFormat | HexFormat | BinaryFormat deriving (Show, Eq, Enum, Bounded)

data Focus = MemTable | RegTable | SourceCode deriving (Eq, Show, Enum, Bounded)

data TableCell = StringCell String | IntCell Int | ValueCell UInt deriving (Show, Eq)

makeLenses ''St

base :: Int -> Int -> Int -> [Int]
base b k x = go k x []
  where go 0 _ r = r
        go l 0  r = go (l - 1) 0 (0 : r)
        go l x' r = go (l - 1) (x' `quot` b) (x' `mod` b : r)

fmtNumber :: NumberFormat -> Int -> String
fmtNumber DecimalFormat = show
fmtNumber HexFormat     = ("0x" ++) . map toHex . base 16 8
  where toHex x
          | 0 <= x && x <= 9   = intToDigit x
          | 10 <= x && x <= 15 = chr (ord 'a' + x - 10)
          | otherwise          = 'f'
fmtNumber BinaryFormat  = ("0b" ++) . map intToDigit . base 2 32

drawLeftAligned :: String -> Widget n
drawLeftAligned = padRight T.Max . str

drawCell :: NumberFormat -> RowMode -> TableCell -> Widget WidgetId
drawCell format Highlighted cell  = withAttr highlightAttr $ drawCell format Normal cell
drawCell _      _ (StringCell s)  = drawLeftAligned $ "  " ++ s
drawCell _      _ (IntCell n)     = drawLeftAligned $ "  " ++ show n
drawCell format _ (ValueCell n)   = drawLeftAligned $ "  " ++ fmtNumber format (fromIntegral n)

drawUI :: St -> [Widget WidgetId]
drawUI st = case st ^. appMode of
  NormalMode -> [ui]
  CycleMode  -> [C.center editBox]

  where editor = st ^. cycleEditor
        errorWidget
          | T.null (st ^. errorMsg) = emptyWidget
          | otherwise               = markup ((st ^. errorMsg) @? errorAttr)

        editBox = B.borderWithLabel (str " Cycles per Step ")  . hLimit 50 $
          C.hCenter errorWidget
          <=> E.renderEditor (str . unlines) False editor
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

getFocus :: St -> Lens' St (Table WidgetId String TableCell)
getFocus St {_widgetFocus = MemTable} = memory
getFocus St {_widgetFocus = RegTable} = register
getFocus St {_widgetFocus = SourceCode} = source

cycleEnum :: (Enum a, Bounded a, Eq a) => a -> a
cycleEnum x = if maxBound == x then minBound else succ x

sendYampaMsg :: MonadIO m => St -> YampaMessage -> m ()
sendYampaMsg st = liftIO . writeBChan (st ^. chanToYampa)

handleVtyEvent :: St -> V.Event -> T.EventM WidgetId (T.Next St)
handleVtyEvent st e =
    case e of
        V.EvKey (V.KChar 'q') []  -> sendYampaMsg st QuitMessage >> M.halt st
        V.EvKey V.KEsc []         -> M.halt st
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

        ev -> traverseOf (getFocus st) (handleTableEvent ev) st >>= M.continue

cycleModeHandleVtyEvent :: St -> V.Event -> T.EventM WidgetId (T.Next St)
cycleModeHandleVtyEvent st (V.EvKey V.KEsc []) = M.continue (st & appMode .~ NormalMode)
cycleModeHandleVtyEvent st (V.EvKey V.KEnter []) = do
  let editorText = unwords . E.getEditContents $ st ^. cycleEditor
  case all isDigit editorText of
    False         -> M.continue $ st & errorMsg .~ "Step size must be a number"
    True
      | n < 0     -> M.continue $ st & errorMsg      .~ "Step size must be a positive number"
      | otherwise -> M.continue $ st & cycleStepSize .~ n
                                     & appMode       .~ NormalMode
                                     & errorMsg      .~ ""
      where n = read editorText

cycleModeHandleVtyEvent st ev = M.continue =<< T.handleEventLensed st cycleEditor E.handleEditorEvent ev

handleEvent :: St -> T.BrickEvent WidgetId AppEvent -> T.EventM WidgetId (T.Next St)
handleEvent st@St {_appMode = CycleMode} (T.VtyEvent ev) = cycleModeHandleVtyEvent st ev

handleEvent st (T.AppEvent (LoadMemory mem)) = do
  let (names, vals) = unzip mem
  M.continue $ st & memory %~ tableReplaceColumn 1 (V.fromList $ map StringCell names)
                  & memory %~ tableReplaceColumn 2 (V.fromList $ map ValueCell vals)

handleEvent st (T.AppEvent (LoadRegistry reg)) = do
  let (names, vals) = unzip reg
  M.continue $ st & register %~ tableReplaceColumn 1 (V.fromList $ map StringCell names)
                  & register %~ tableReplaceColumn 2 (V.fromList $ map ValueCell vals)

handleEvent st (T.AppEvent (ReloadEvent src reg mem)) = M.continue $ initialState (st ^. chanToYampa) src reg mem

handleEvent st (T.AppEvent (ChangeEvent cycles archComp)) =
  let updateData _   Nothing  t = t
      updateData dat (Just r) t = t & tableCellT r 2 .~ ValueCell (S.index dat r)
                                    & tableSetHighlight [r]
      lnM
        | st ^. clockCycles > 0 = archLineNum archComp
        | otherwise             = Nothing

  in  M.continue $ st & clockCycles %~ (+ cycles)
                      & register %~ updateData  (archReg archComp) (archRegChange archComp)
                      & memory   %~ updateData  (archMem archComp) (archMemChange archComp)
                      & source   %~ tableMoveTo (lnM)

handleEvent st (T.VtyEvent e) = handleVtyEvent st e
handleEvent st _              = M.continue st

initialState :: BChan YampaMessage -> String -> [(String, UInt)] -> [(String, UInt)] -> St
initialState chan src reg mem =
  let src' = lines src
      regTable = makeTable RegTableId RegColId 1 [
                ("#"    , 6,  V.fromList . map IntCell    $ take (length reg) [0..]),
                ("Alias", 10, V.fromList . map StringCell $ map fst reg),
                ("Value", 40, V.fromList . map ValueCell  $ map snd reg)
               ]
      memTable = makeTable MemTableId MemColId 1 [
                ("#"    , 6,  V.fromList . map IntCell    $ take (length mem) [0..]),
                ("Alias", 10, V.fromList . map StringCell $ map fst mem),
                ("Value", 40, V.fromList . map ValueCell  $ map snd mem)
               ]
      sourceTable = makeTable SourceTableId SourceColId 1 [
                ("#"    , 6,  V.fromList . map IntCell    $ take (length src') [1..]),
                (" ", 10000,  V.fromList . map StringCell $ src')
               ]

      editor = E.editor CycleEditorId (Just 1) "1"
  in St
     { _memory = memTable
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

theApp :: M.App St AppEvent WidgetId
theApp = M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = handleEvent
  , M.appStartEvent = return
  , M.appAttrMap = const theMap
  }
