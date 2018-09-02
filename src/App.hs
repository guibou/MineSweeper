{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -Wall -Wno-partial-type-signatures #-}
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

newGameRandom :: Size -> Int -> UTCTime -> FieldState
newGameRandom size nbMines time = newGame (truncate $ utcTimeToPOSIXSeconds time) size nbMines

header :: _ => Dynamic t GameStatus -> Dynamic t UTCTime -> m (Event t ())
header gameStatus timer = do
  divClass "header" $ do
    restartEvt <- elClass "span" "restart" $ button "Restart"
    divClass "timer" $ text "Time: " >> dynText (toSec <$> gameStatus <*> timer)
    divClass "mineCount" $ text "Mines: " >> text "20"
    pure restartEvt

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

  restartEvt <- header gameStatus timer

  let newGameEvent = newGameRandom size nbMines <$> (current timer <@ restartEvt)

  let gameEvents = leftmost [
        play <$> e,
        const <$> newGameEvent
        ]
  game <- foldDyn ($) randomGame gameEvents
  e <- mineSweeperWidget size game
  pure ()

nbsp :: Text
nbsp = " "

cell :: _ => Coord -> Dynamic t (Bool, Status) -> m (Event t Coord)
cell coord st' = do
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

mineSweeperWidget :: _ => Size -> Dynamic t FieldState -> m (Event t Coord)
mineSweeperWidget size fieldDyn = leftmost . mconcat <$> do
  el "table" $ do
    for (allCells size) $ \line -> do
      el "tr" $ do
        for line $ \coord -> do
          cell coord (getFieldStatus coord <$> fieldDyn)
