{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module UIReflex.UI where

import MineSweeper
import UIReflex.CSS
import UIReflex.LongClicks

import Control.Monad.IO.Class
import Reflex.Dom
import Data.Traversable (for)

import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Semigroup


import qualified Data.Text as Text


number :: Show a => a -> Text
number i = Text.pack $ show i

toSec :: GameStatus -> UTCTime -> Text
toSec (Terminated t) _ = Text.pack (show @Int (truncate t))
toSec NotRunning _now = "-"
toSec (Running started) now = Text.pack (show @Int (truncate $ now `diffUTCTime` started))

data GameStatus = NotRunning
                | Running UTCTime
                | Terminated NominalDiffTime
                deriving (Show)

startOrTerminateGame :: (UTCTime, GameState) -> GameStatus -> GameStatus
startOrTerminateGame (now, _) NotRunning = Running now
startOrTerminateGame (now, g) (Running t) = case gameResult g of
  Done _ -> Terminated (now `diffUTCTime` t)
  _ -> Running t
startOrTerminateGame _ r = r

newGameRandom :: Size -> Int -> UTCTime -> GameState
newGameRandom size nbMines time = newGame (truncate $ utcTimeToPOSIXSeconds time) size nbMines

header :: MonadWidget t m => Dynamic t GameState -> Dynamic t GameStatus -> Dynamic t UTCTime -> m (Event t ())
header fs gameStatus timer = do
  divClass "header" $ do
    restartEvt <- elClass "span" "restart" $ button "Restart"
    divClass "timer" $ text "Time: " >> dynText (toSec <$> gameStatus <*> timer)
    divClass "mineCount" $ text "Mines: " >> text "20"

    text "Status: " >> display (gameResult <$> fs)

    pure restartEvt

go :: IO ()
go = mainWidgetWithCss css $ mdo
  -- TODO: size is not in the right order
  let size = Size 9 17
      nbMines = 20
      randomGame = newGameRandom size nbMines now

  now <- liftIO getCurrentTime
  timer <- fmap _tickInfo_lastUTC <$> clockLossy 0.5 now

  gameStatus <- foldDyn ($) NotRunning (leftmost
                                       [
                                         startOrTerminateGame <$> ((,) <$> current timer <@> (Reflex.Dom.traceEvent "updated" $ updated game)),
                                         const NotRunning <$ restartEvt
                                       ])

  restartEvt <- header game gameStatus timer

  let newGameEvent = newGameRandom size nbMines <$> (current timer <@ restartEvt)

  let gameEvents = leftmost [
        uncurry (flip play) <$> e,
        const <$> newGameEvent
        ]
  game <- foldDyn ($) randomGame gameEvents
  e <- mineSweeperWidget size game
  pure ()

cell :: _ => Coord -> Dynamic t (Visibility, CaseContent) -> m (Event t (Coord, MineAction))
cell coord st' = do
  st <- holdUniqDyn st'

  e <- dyn . ffor st $ \status -> mdo
    let (visibility, innerStatus) = status
        dataStatus = case innerStatus of
          Bomb -> mempty
          SafeArea i -> "data-number" =: Text.pack (show i)
        clsVisibility = case visibility of 
          Hidden NotFlagged -> "hidden unknown"
          Hidden Flagged -> "hidden flagged"
          Visible -> "visible"
        clsContent = case innerStatus of
          Bomb -> "bomb"
          SafeArea i -> "safe"
    (td, _) <- elClass' "td" (clsVisibility <> " " <> clsContent) $ elAttr "span" dataStatus $ blank
    longClick <- longClickEvent td
    let actionEvt = click2Action <$> longClick
    pure ((coord,) <$> actionEvt)
  switchHold never e

click2Action :: ClickType -> MineAction
click2Action LongClick = Flag
click2Action ShortClick = Reveal

clsStatus :: GameResult -> Text
clsStatus (Done Win) = "win"
clsStatus (Done Lose) = "lose"
clsStatus (Playing _) = "playing"

mineSweeperWidget :: _ => Size -> Dynamic t GameState -> m (Event t (Coord, MineAction))
mineSweeperWidget size fieldDyn = leftmost . mconcat <$> do
  let dynStatus = clsStatus . gameResult <$> fieldDyn

  elDynClass "table" dynStatus $ do
    for (allCells size) $ \line -> do
      el "tr" $ do
        for line $ \coord -> do
          cell coord (caseStatus coord <$> fieldDyn)
