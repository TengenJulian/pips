{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TUI.Table
  ( module TUI.Table
  ) where

import Brick.Types
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Core ( (<+>) , (<=>) , hLimit , emptyWidget, withDefAttr )

import Data.List (intersperse)
import qualified Data.Vector as V

import Brick.Widgets.Core (withDefAttr)
import Brick.Types
import Brick.Widgets.List
import Brick.Widgets.Border

import Graphics.Vty.Input.Events

import Lens.Micro
import Lens.Micro.TH

data Table n h e = Table  {
  _tableName :: n
  , _rowWithHighlights :: [Int]
  , _tableLength :: Int
  , _tableSelHighlightDisabled :: Bool
  , _tableLists :: [(h, Int, List n (RowMode, e))]
  }

data RowMode = Selected | Highlighted | Normal deriving (Show, Eq)

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0..]

makeTable :: forall n h e. n -> (Int -> n) -> Int -> [(h, Int, V.Vector e)] -> Table n h e
makeTable name listName' size = Table name [] size False . imap convertTriple
  where convertTriple :: Int -> (h, Int, V.Vector e) -> (h, Int, List n (RowMode, e))
        convertTriple i = (& _3 %~ toListWidget i)

        toListWidget i v = list (listName' i) (V.map (\x -> (Normal, x)) v) size

makeLenses ''Table

  -- basicly apply event handler on all the third elements of the list of triples.
  -- The elemens of type List e have an, instance of HandleEvent.
  -- The monad stuff is handled by traverseOf.
handleTableEvent :: Ord n => Event -> Table n h e -> EventM n (Table n h e)
handleTableEvent e = traverseOf (tableLists . each . _3) (handleListEvent e)

-- | A Lens that focuses on a particular element in the Vector
vix :: Int -> Lens' (V.Vector a) a
vix i = lens (V.! i) (\v x -> v V.// [(i, x)])

renderTable :: (Show n, Ord n) => Table n h e -> (h -> Widget n) -> (RowMode -> e -> Widget n) -> Widget n
renderTable table renderHeader renderElem =
  let lists = table' ^. tableLists
      writeHighlights t i = t & tableLists . each . _3
                              . listElementsL . vix i . _1 .~ Highlighted

      table' = foldl writeHighlights table (table ^. rowWithHighlights)
      re _ (mode, elem')
        | table ^. tableSelHighlightDisabled = withDefAttr listAttr $ renderElem mode elem'
        | otherwise                          = renderElem mode elem'

      renderColumn _ (h, width, ls) = hLimit width $
                                      renderHeader h
                                      <=> hBorder
                                      <=> renderList re True ls

  in foldl (<+>) emptyWidget . intersperse vBorder . imap renderColumn $ lists

transformTableCell :: Table n a e ->  Int -> Int -> (e -> e) -> Table n a e
transformTableCell table r c f = table & tableLists . ix c %~ (& _3 . listElementsL . vix r . _2 %~ f)

tableMoveTo :: Maybe Int -> Table n a e -> Table n a e
tableMoveTo Nothing  t = t & tableSelHighlightDisabled .~ True
tableMoveTo (Just r) t = t & tableSelHighlightDisabled .~ False
                           & tableLists %~ map (& _3 %~ listMoveTo r)

partsOf :: Lens' a b -> Lens' [a] [b]
partsOf l = lens (^.. each . l) setter
  where setter = zipWith (\old new -> old & l .~ new)

vecElemsL :: Lens' (V.Vector a) [a]
vecElemsL f s = V.fromList <$> f (V.toList s)

-- A traversal for the elemens in the c-th column.
-- For example if table :: Table String Int
-- table & tableCellT 0 . ix 1 .~ 3. This replaces the elemen in column 0, row 1 with 3.
-- table & tableCellT 0 . each %~ succ. This maps succ over all the elemens in first column.
-- tableCellT :: Int -> Table n a e -> Travers
partsOfVec :: Lens' (V.Vector e) (V.Vector e)
partsOfVec = lens id replace
  where replace old new
          | sl > 0 = new V.++ V.slice (V.length new) sl old
          | otherwise = V.slice 0 (V.length old) new
          where sl = max 0 $ V.length new - V.length old

tableCellT :: Applicative f => Int -> ([e] -> f [e]) -> Table n h e -> f (Table n h e)
tableCellT c = tableLists . ix c . _3 . listElementsL . vecElemsL . partsOf _2

tableAddHighlight :: Table n e h -> [Int] -> Table n e h
tableAddHighlight table xs = table & rowWithHighlights %~ (++ xs)

tableSetHighlight :: Table n e h -> [Int] -> Table n e h
tableSetHighlight table xs = table & rowWithHighlights .~ xs