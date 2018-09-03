{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module UIReflex.UI where

import MineSweeper
import UIReflex.CSS

import Control.Monad.IO.Class
import Reflex.Dom
import Data.Traversable (for)

import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Bool (bool)

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

header :: MonadWidget t m => Dynamic t GameState -> Dynamic t GameStatus -> Dynamic t UTCTime -> m (Event t (), Dynamic t MineAction)
header fs gameStatus timer = do
  divClass "header" $ do
    restartEvt <- elClass "span" "restart" $ button "Restart"
    divClass "timer" $ text "Time: " >> dynText (toSec <$> gameStatus <*> timer)
    divClass "mineCount" $ text "Mines: " >> text "20"
    mineAction <- flagToolbar never

    text "Status: " >> display (gameResult <$> fs)

    pure (restartEvt, mineAction)

flagToolbar :: MonadWidget t m => Event t () -> m (Dynamic t MineAction)
flagToolbar _eventReset = do
  text "Flag: "
  check <- checkbox False def
  pure (bool Reveal Flag <$> value check)

go :: IO ()
go = mainWidgetWithCss css $ mdo
  let size = Size 15 15
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

  (restartEvt, mineAction) <- header game gameStatus timer

  let newGameEvent = newGameRandom size nbMines <$> (current timer <@ restartEvt)

  let gameEvents = leftmost [
        play <$> current mineAction <@> e,
        const <$> newGameEvent
        ]
  game <- foldDyn ($) randomGame gameEvents
  e <- mineSweeperWidget size game
  pure ()

nbsp :: Text
nbsp = " "

cell :: _ => Coord -> Dynamic t (Visibility, CaseContent) -> m (Event t Coord)
cell coord st' = do
  st <- holdUniqDyn st'

  e <- dyn . ffor st $ \status -> mdo
    let (visibility, innerStatus) = status
        dataStatus = case innerStatus of
          Bomb -> mempty
          SafeArea i -> "data-number" =: Text.pack (show i)
        (clsVisibility, evt) = case visibility of 
          Hidden NotFlagged -> ("hidden unknown", coord <$ domEvent Click td)
          Hidden Flagged -> ("hidden flagged", (coord <$ domEvent Click td))
          Visible -> ("visible", never)
    (td, _) <- elClass' "td" clsVisibility $ elAttr "span" dataStatus $ text nbsp
    pure evt
  switchHold never e

clsStatus :: GameResult -> Text
clsStatus Win = "win"
clsStatus Lose = "lose"
clsStatus (Current _) = "playing"

mineSweeperWidget :: _ => Size -> Dynamic t GameState -> m (Event t Coord)
mineSweeperWidget size fieldDyn = leftmost . mconcat <$> do
  let dynStatus = clsStatus . gameResult <$> fieldDyn

  elDynClass "table" dynStatus $ do
    for (allCells size) $ \line -> do
      el "tr" $ do
        for line $ \coord -> do
          cell coord (caseStatus coord <$> fieldDyn)
