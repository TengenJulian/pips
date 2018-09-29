{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Tui.Table
  ( module Tui.Table
  ) where

import Brick.Types
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Core ( (<=>) , hLimit , withDefAttr, hBox )

import Data.List (intersperse)
import qualified Data.Vector as V

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

makeLenses ''Table

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0..]

makeTable :: forall n h e. n -> (Int -> n) -> Int -> [(h, Int, V.Vector e)] -> Table n h e
makeTable name listName' size = Table name [] size False . imap convertTriple
  where convertTriple :: Int -> (h, Int, V.Vector e) -> (h, Int, List n (RowMode, e))
        convertTriple i = (& _3 %~ toListWidget i)

        toListWidget i v = list (listName' i) (V.map (\x -> (Normal, x)) v) size

handleTableEvent :: Ord n => Event -> Table n h e -> EventM n (Table n h e)
handleTableEvent e = traverseOf (tableLists . each . _3) (handleListEvent e)

-- | A Lens that focuses on a particular element in the Vector
vix :: Int -> Lens' (V.Vector a) a
vix i = lens (V.! i) (\v x -> v V.// [(i, x)])

renderTable :: (Show n, Ord n) => Table n h e -> (h -> Widget n) -> (RowMode -> e -> Widget n) -> Widget n
renderTable table renderHeader renderElem =
  let table' = foldl writeHighlights table (table ^. rowWithHighlights)
      writeHighlights t i = t & tableLists . each . _3
                              . listElementsL . vix i . _1 .~ Highlighted

      re _ (mode, elem')
        | table ^. tableSelHighlightDisabled = withDefAttr listAttr $ renderElem mode elem'
        | otherwise                          = renderElem mode elem'

      renderColumn (h, width, ls) = hLimit width $
                                      renderHeader h
                                      <=> hBorder
                                      <=> renderList re True ls

  in  hBox . intersperse vBorder . map renderColumn $ table' ^. tableLists

tableMoveTo :: Maybe Int -> Table n a e -> Table n a e
tableMoveTo Nothing  t = t & tableSelHighlightDisabled .~ True
tableMoveTo (Just r) t = t & tableSelHighlightDisabled .~ False
                           & tableLists %~ map (& _3 %~ listMoveTo r)

tableCellT :: Int -> Int -> Traversal' (Table n h e) e
tableCellT r c = tableLists . ix c . _3 . listElementsL . vix r . _2

tableReplaceColumn :: Int -> V.Vector e -> Table n h e -> Table n h e
tableReplaceColumn c new table = table & tableLists . ix c . _3 %~ replace
  where new' = V.map (\x -> (Normal, x)) new

        replace ls = listReplace new' (listSelected ls) ls

tableAddHighlight :: [Int] -> Table n e h -> Table n e h
tableAddHighlight xs = rowWithHighlights %~ (++ xs)

tableSetHighlight :: [Int] -> Table n e h -> Table n e h
tableSetHighlight xs = rowWithHighlights .~ xs
