{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
module UIReflex.LongClicks
  ( longClickEvent
  , ClickType(..)
  ) where

import Data.Time.Clock
import Reflex.Dom
import Control.Monad.IO.Class

-- | if the click in long or short
data ClickType = LongClick | ShortClick
  deriving (Show)

data Evt = Up | Down
  deriving (Show)

data Status = Nop | Waiting UTCTime | ClickDetected ClickType
  deriving Show

-- | register the event long click for a widget
longClickEvent :: _ => e -> m (Event t ClickType)
longClickEvent w = do
  let
    -- The order is important because we prioritize 'Up' event
    evt = leftmost [
      Up <$ domEvent Mouseup w,
      Up <$ domEvent Touchend w,
      Down <$ domEvent Mousedown w,
      Down <$ domEvent Touchstart w
      ]

  evtWithTime <- performEvent (liftIO . tagTime <$> evt)

  d <- foldDyn (flip m) Nop evtWithTime
  pure (fmapMaybe onlyClicks (updated d))

onlyClicks :: Status -> Maybe ClickType
onlyClicks (ClickDetected c) = Just c
onlyClicks _ = Nothing

m :: Status -> (UTCTime, Evt) -> Status
m stat (t, e) = case (stat, e) of
  (Nop, Up) -> Nop
  (ClickDetected _, Up) -> Nop
  (_, Down) -> Waiting t
  (Waiting t', Up) -> let diff = t `diffUTCTime` t' in
                      if diff > 0.09
                      then ClickDetected LongClick
                      else ClickDetected ShortClick
    
tagTime :: b -> IO (UTCTime, b)
tagTime e = do
  t <- getCurrentTime
  pure (t, e)
