{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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

newGameRandom :: Size -> Int -> UTCTime -> FieldState
newGameRandom size nbMines time = newGame (truncate $ utcTimeToPOSIXSeconds time) size nbMines

header :: MonadWidget t m => Dynamic t FieldState -> Dynamic t GameStatus -> Dynamic t UTCTime -> m (Event t (), Dynamic t MineAction)
header fs gameStatus timer = do
  divClass "header" $ do
    restartEvt <- elClass "span" "restart" $ button "Restart"
    divClass "timer" $ text "Time: " >> dynText (toSec <$> gameStatus <*> timer)
    divClass "mineCount" $ text "Mines: " >> text "20"
    mineAction <- flagToolbar never

    text "Status: " >> display (getGameStatus <$> fs)

    pure (restartEvt, mineAction)

flagToolbar :: MonadWidget t m => Event t () -> m (Dynamic t MineAction)
flagToolbar _eventReset = do
  text "Flag: "
  check <- checkbox False def
  pure (bool Open Flag <$> value check)

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
        (\mode -> play . (,mode)) <$> current mineAction <@> e,
        const <$> newGameEvent
        ]
  game <- foldDyn ($) randomGame gameEvents
  e <- mineSweeperWidget size game
  pure ()

nbsp :: Text
nbsp = " "

cell :: _ => Coord -> Dynamic t (StatusModifier, Status) -> m (Event t Coord)
cell coord st' = do
  st <- holdUniqDyn st'

  e <- dyn . ffor st $ \status -> mdo
    let (visibility, innerStatus) = status
        dataStatus = case innerStatus of
          Bomb -> mempty
          SafeArea i -> "data-number" =: Text.pack (show i)
        (clsVisibility, evt) = case visibility of 
          Hidden Unknown -> ("hidden unknown", coord <$ domEvent Click td)
          Hidden Flagged -> ("hidden flagged", (coord <$ domEvent Click td))
          Visible -> ("visible", never)
    (td, _) <- elClass' "td" clsVisibility $ elAttr "span" dataStatus $ text nbsp
    pure evt
  switchHold never e

mineSweeperWidget :: _ => Size -> Dynamic t FieldState -> m (Event t Coord)
mineSweeperWidget size fieldDyn = leftmost . mconcat <$> do
  el "table" $ do
    for (allCells size) $ \line -> do
      el "tr" $ do
        for line $ \coord -> do
          cell coord (getFieldStatus coord <$> fieldDyn)
