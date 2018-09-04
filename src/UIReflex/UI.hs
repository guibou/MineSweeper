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

import qualified Data.Text as Text


number :: Show a => a -> Text
number i = Text.pack $ show i

toSec :: GameStatus -> UTCTime -> Text
toSec NotRunning _now = "-"
toSec (Running started) now = Text.pack (show @Int (truncate $ now `diffUTCTime` started))

data GameStatus = NotRunning
                | Running UTCTime
                deriving (Show)

startByEvent :: UTCTime -> GameStatus -> GameStatus
startByEvent now NotRunning = Running now
startByEvent _ r = r

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
  ticks <- tickLossy 0.5 now -- two time the sampling value ;)
  timer <- holdDyn now (_tickInfo_lastUTC <$> ticks)

  gameStatus <- foldDyn ($) NotRunning (leftmost
                                       [
                                         startByEvent <$> (current timer <@ e),
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

nbsp :: Text
nbsp = " "

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
    (td, _) <- elClass' "td" clsVisibility $ elAttr "span" dataStatus $ text nbsp
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
clsStatus Playing = "playing"

mineSweeperWidget :: _ => Size -> Dynamic t GameState -> m (Event t (Coord, MineAction))
mineSweeperWidget size fieldDyn = leftmost . mconcat <$> do
  let dynStatus = clsStatus . gameResult <$> fieldDyn

  elAttr "div" ("onContextMenu" =: "function (){ return false; }") $ do
    elDynClass "table" dynStatus $ do
      for (allCells size) $ \line -> do
        el "tr" $ do
          for line $ \coord -> do
            cell coord (caseStatus coord <$> fieldDyn)
