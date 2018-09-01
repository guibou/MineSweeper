{-# OPTIONS -Wall #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module App where

import MineSweeper
import CSS

import Control.Monad.IO.Class
import Reflex.Dom
import Data.Traversable (for)
import Data.Semigroup ((<>))
import Control.Monad (void)

import Data.Text (Text)

import qualified Data.Text as Text

number :: Show a => a -> Text
number i = Text.pack $ show i

go :: IO ()
go = mainWidgetWithCss css $ mdo
  let size = Size 15 15
  randomGame <- liftIO $ newGame size 20

  game <- foldDyn play randomGame e
  e <- mineSweeperWidget size game
  pure ()

nbsp :: Text
nbsp = " "

cell :: MonadWidget t m => Coord -> Dynamic t (Bool, Status) -> m (Event t Coord)
cell coord st' = mdo
  st <- holdUniqDyn st'

  e <- dyn . ffor st $ \status -> mdo
    let (cls, t, e) = case status of
          (False, _) -> ("hidden", nbsp, coord <$ domEvent Click td)
          (True, c) -> case c of
            Bomb -> ("bomb", "B", never)
            SafeArea 0 -> ("empty", nbsp, never)
            SafeArea i -> ("safe" <> number i, number i, never)
    (td, _) <- elClass' "td" cls (text t)
    pure e
  switchHold never e

mineSweeperWidget :: MonadWidget t m => Size -> Dynamic t FieldState -> m (Event t Coord)
mineSweeperWidget size fieldDyn = leftmost . mconcat <$> do
  el "table" $ do
    for (allCells size) $ \line -> do
      el "tr" $ do
        for line $ \coord -> do
          cell coord (getFieldStatus coord <$> fieldDyn)
