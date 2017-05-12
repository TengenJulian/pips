{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module TUI.Table
  ( module TUI.Table
  ) where

import Control.Lens

import Data.List (intersperse)

import qualified Data.Vector as V
import Data.Vector.Lens

import Brick.Types
import Brick.Widgets.List
import Brick.Widgets.Border

import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , emptyWidget
  )

data Table h e = Table  {
  _tableName :: Name
  , _rowWithHighlights :: [Int]
  , _tableLists :: [(h, Int, List (RowMode, e))]
  }

data RowMode = Selected | Highlighted | Normal deriving (Show, Eq)

makeTable :: Name -> Int -> [(h, Int, V.Vector e)] -> Table h e
makeTable name@(Name nameStr) size = Table name [] . imap convertTriple
  where convertTriple :: Int -> (h, Int, V.Vector e) -> (h, Int, List (RowMode, e))
        convertTriple i = (& _3 %~ (\v -> list (Name $ nameStr ++ "list" ++ show i) (V.map (\x -> (Normal, x))v) size))

makeLenses ''Table

instance HandleEvent (Table h e) where
  -- basicly apply event handler on all the third elements of the list of triples.
  -- The elemens of type List e have an, instance of HandleEvent.
  -- The monad stuff is handled by traverseOf.
  handleEvent e = traverseOf (tableLists . each . _3) (handleEvent e)

renderTable :: Table h e -> (h -> Widget) -> (RowMode -> e -> Widget) -> Widget
renderTable table renderHeader renderElem =
  let lists = table' ^. tableLists
      writeHighlights table i = table & tableLists . each . _3 . listElementsL . from vector . ix i . _1 .~ Highlighted
      table' = foldl writeHighlights table (table ^. rowWithHighlights)
      renderColumn _ (h, width, ls) = hLimit width $
                                      renderHeader h
                                      <=> hBorder
                                      <=> renderList ls (\_ (mode, elem) -> renderElem mode elem)
  in foldl (<+>) emptyWidget . intersperse vBorder . imap renderColumn $ lists

transformTableCell :: Table a e ->  Int -> Int -> (e -> e) -> Table a e
-- the & seems to make a focus on some part of the table. %~ transforms this focus.
transformTableCell table r c f= table & tableLists . ix c %~ (& _3 . listElementsL . ix r . _2 %~ f)

tableMoveTo :: Int -> Table a e -> Table a e
tableMoveTo s = (& tableLists %~ map (& _3 %~ listMoveTo s))

-- A traversal for the elemens in the c-th column.
-- For example if table :: Table String Int
-- table & tableCellT 0 . ix 1 .~ 3. This replaces the elemen in column 0, row 1 with 3.
-- table & tableCellT 0 . each %~ succ. This maps succ over all the elemens in first column.
tableCellT :: Int -> Traversal' (Table h e) [e]
tableCellT c = tableLists . ix c . _3 . listElementsL . from vector . partsOf (each . _2)

tableAddHighlight :: Table e h -> [Int] -> Table e h
tableAddHighlight table xs = table & rowWithHighlights %~ (++ xs)

tableSetHighlight :: Table e h -> [Int] -> Table e h
tableSetHighlight table xs = table & rowWithHighlights .~ xs
