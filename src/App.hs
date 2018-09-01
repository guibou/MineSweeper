{-# LANGUAGE OverloadedStrings #-}
module App where

import CSS

import Reflex.Dom

go :: IO ()
go = mainWidgetWithCss css $ do
  _ <- button "click"
  pure ()
