{-# LANGUAGE OverloadedStrings #-}
module UIReflex.CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay
import Clay.Stylesheet

import Data.ByteString (ByteString)

css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  star ? do
    padding (px 0) 0 0 0
    margin (px 0) 0 0 0
    borderWidth (px 0)
    "--gridSizeW" -: "17"
    "--gridSizeH" -: "9"
    "--headerSize" -: "5vh"

  body ? do
    margin (pct 0) 0 0 0

  ".header" ? do
    display flex
    "height" -: "var(--headerSize)"

    ".mineCount" ? do
      fontColor red
    ".timer" ? do
      keyframes "expand" [(0, Clay.opacity 0)]

  -- custom override for all winners
  table # ".win" ? do
    padding auto auto auto auto
    td # ".unknown" ? span # ":after" ? do -- not flagged bomb
      content (stringContent "💣") -- No number, you are a bomb

  -- custom override for losers
  table # ".lose" ? do
    td # ".flagged" ? do
      backgroundColor lightcoral -- wrong flag

    td # ".visible.bomb" ? do
      backgroundColor lightcoral -- visible bomb
    pure ()

  table ? do
    --borderCollapse collapse
    borderSpacing (px 0)
    borderWidth (px 0)

    td # ".flagged" ? span # ":after" ? do
      content (stringContent "🚩")

    td ? do
      boxSizing borderBox
      "width" -: "calc(100vw / var(--gridSizeW))"
      "height" -: "calc(0.99 * (100vh - var(--headerSize)) / var(--gridSizeH))"
      fontFamily [] [monospace]
      borderWidth (vmin 0.1)
      "font-height" -: "1vmin"
      borderStyle solid
      borderColor grey
      backgroundColor lightgrey
      textAlign center
      verticalAlign middle

      keyframes "expand" [(0, Clay.opacity 0)]

    ".hidden" ? do
      borderColor4 gainsboro dimgrey dimgrey gainsboro
      borderWidth (vmin 1.5)
  table # ".playing" ? td # ".visible" ? displays
  table # ".win" ? td # ".visible" ? displays
  table # ".lose" ? td ? displays

displays :: StyleM ()
displays = do
      span # ":after" ? do
        content (stringContent "💣") -- No number, you are a bomb
      span # "@data-number" # ":after" ? do
        content (attrContent "data-number")
      span # ("data-number" @= "0") # ":after" ? do
        content normal
      span # ("data-number" @= "1") ? color blue
      span # ("dataonumber" @= "2") ? color green
      span # ("data-number" @= "3") ? color red
      span # ("data-number" @= "4") ? color orange
      span # ("data-number" @= "5") ? color yellow
      span # ("data-number" @= "6") ? color purple
      span # ("data-number" @= "7") ? color pink
      span # ("data-number" @= "8") ? color black
