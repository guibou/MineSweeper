{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module UIReflex.UI (go) where

import MineSweeper
import UIReflex.CSS
import UIReflex.LongClicks

import Control.Monad.IO.Class
import Reflex.Dom
import Data.Traversable (for)

import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX

import qualified Data.Text as Text

newGameRandom :: Size -> Int -> UTCTime -> GameState
newGameRandom size nbMines time = newGame (truncate $ utcTimeToPOSIXSeconds time) size nbMines

endGameBanner :: MonadWidget t m => m (Event t ())
endGameBanner = do
  (dom, _) <- elClass' "div" "banner" $ do
    text "click to restart"

  pure $ domEvent Click dom

go :: IO ()
go = mainWidgetWithCss css $ mdo
  let dynStatus = clsStatus . gameResult <$> game

  game <- elDynClass "div" dynStatus $ mdo
    -- TODO: size is not in the right order
    let size = Size 9 17
        nbMines = 20
        randomGame = newGameRandom size nbMines now

    now <- liftIO getCurrentTime

    restartEvt <- endGameBanner

    currentRestartTime <- performEvent ((\_ -> liftIO getCurrentTime) <$> restartEvt)
    let newGameEvent = newGameRandom size nbMines <$> currentRestartTime

    let gameEvents = leftmost [
          uncurry (flip play) <$> e,
          const <$> newGameEvent
          ]
    game <- foldDyn ($) randomGame gameEvents
    e <- mineSweeperWidget size game
    pure game
  pure ()

cell :: MonadWidget t m => Coord -> Dynamic t (Visibility, CaseContent) -> m (Event t (Coord, MineAction))
cell coord st' = do
  st <- holdUniqDyn st'

  let (cellClass, spanAttr) = splitDynPure $ ffor st $ \case
        (visibility, innerStatus) -> (clsVisibility <> " " <> clsContent, dataStatus)
          where
            (dataStatus, clsContent) = case innerStatus of
              Bomb -> (mempty, "bomb")
              SafeArea i -> ("data-number" =: Text.pack (show i), "safe")
            clsVisibility = case visibility of
              Hidden NotFlagged -> "hidden unknown"
              Hidden Flagged -> "hidden flagged"
              Visible -> "visible"

  (cellEvent, _) <- elDynClass' "div" cellClass $ elDynAttr "span" (spanAttr) $ blank
  longClick <- longClickEvent cellEvent
  let actionEvt = click2Action <$> longClick

  pure ((coord,) <$> actionEvt)

click2Action :: ClickType -> MineAction
click2Action LongClick = Flag
click2Action ShortClick = Reveal

clsStatus :: GameResult -> Text
clsStatus (Done Win) = "win"
clsStatus (Done Lose) = "lose"
clsStatus (Playing _) = "playing"

mineSweeperWidget :: MonadWidget t m => Size -> Dynamic t GameState -> m (Event t (Coord, MineAction))
mineSweeperWidget size fieldDyn = leftmost . mconcat <$> do
  divClass "grid" $ do
    for (allCells size) $ \line -> do
        for line $ \coord -> do
          cell coord (caseStatus coord <$> fieldDyn)
