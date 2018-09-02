{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay

import Data.ByteString (ByteString)

css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  ".header" ? do
    display flex

    ".mineCount" ? do
      fontColor red
    ".timer" ? do
      animationName "expand"
      animationDuration (sec 0.5)

      keyframes "expand" [(0, Clay.opacity 0)]
      
  table ? do
    width (pct 100)
    height (pct 100)
    borderCollapse collapse
    "table-layout" -: "fixed"

    td # ".flagged" ? span # ":after" ? do
      content (stringContent "F")

    td # ".visible" ? do
      span # ":after" ? do
        content (stringContent "B") -- No number, you are a bomb
      span # "@data-number" # ":after" ? do
        content (attrContent "data-number")
      span # ("data-number" @= "0") # ":after" ? do
        content normal
      span # ("data-number" @= "1") ? color blue
      span # ("data-number" @= "2") ? color green
      span # ("data-number" @= "3") ? color red
      span # ("data-number" @= "4") ? color orange
      span # ("data-number" @= "5") ? color yellow
      span # ("data-number" @= "6") ? color purple
      span # ("data-number" @= "7") ? color pink
      span # ("data-number" @= "8") ? color black
    td ? do
      fontFamily [] [monospace]
      borderWidth (px 1)
      borderStyle solid
      borderColor grey
      backgroundColor lightgrey
      textAlign center
      verticalAlign middle

      animationName "expand"
      animationDuration (sec 0.5)

      keyframes "expand" [(0, Clay.opacity 0)]

    ".hidden" ? do
      backgroundColor dimgrey

