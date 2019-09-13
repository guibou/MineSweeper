{-# LANGUAGE OverloadedStrings #-}
module UIReflex.CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay
import Clay.Stylesheet

import Data.ByteString (ByteString)
import Clay.Media (screen)


css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  query screen [Feature "orientation" (Just "portrait")] $ do
    ".grid" ? do
      "grid-auto-flow" -: "column"
      "grid-template-columns" -: "repeat(var(--gridSizeH), 1fr)"
      "grid-template-rows" -: "repeat(var(--gridSizeW), 1fr)"

  star ? do
    padding (px 0) 0 0 0
    margin (px 0) 0 0 0
    borderWidth (px 0)
    "--gridSizeW" -: "17"
    "--gridSizeH" -: "9"
    "--headerSize" -: "0vh"

  body ? do
    margin (pct 0) 0 0 0

  ".header" ? do
    display flex
    "height" -: "var(--headerSize)"

    ".mineCount" ? do
      fontColor red

  -- custom override for all winners
  ".win" ? ".grid" ? do
    padding auto auto auto auto
    div # ".unknown" ? span # ":after" ? do -- not flagged bomb
      content (stringContent "💣") -- No number, you are a bomb

  -- custom override for losers
  ".lose" ? ".grid" ? do
    div # ".flagged" ? do
      backgroundColor lightcoral -- wrong flag

    div # ".visible.bomb" ? do
      backgroundColor lightcoral -- visible bomb
    pure ()

  ".grid" ? do
    width (vw 100)
    height (vh 100)
    display grid
    "align-items" -: "stretch";
    "grid-template-columns" -: "repeat(var(--gridSizeW), 1fr)"
    "grid-template-rows" -: "repeat(var(--gridSizeH), 1fr)"

    div # ".flagged" ? span # ":after" ? do
      content (stringContent "🚩")

    div ? do
      fontFamily [] [monospace]
      borderWidth (vmin 0.1)
      borderStyle solid
      borderColor grey
      backgroundColor lightgrey
      display grid
      "align-items" -: "center";
      textAlign center


    ".hidden" ? do
      borderColor4 gainsboro dimgrey dimgrey gainsboro
      borderWidth (vmin 1.5)

  ".playing" ? ".grid" ? div # ".visible" ? displays
  ".win" ? ".grid" ? div # ".visible" ? displays
  ".lose" ? ".grid" ? div ? displays

  ".banner" ? do
    position fixed
    top (pct 50)
    left (pct 50)
    "transform" -: "translate(-50%, -50%)"
    borderStyle solid
    borderWidth (px 1)
    display none
    fontSize (vh 10)

  ".win" ? ".banner" ? do
    backgroundColor (setA 0.4 (toRgba lightgreen))
    borderColor green
    display block
  ".win" ? ".banner" # ":before" ? do
    content (stringContent "Winner: ")

  ".lose" ? ".banner" ? do
    backgroundColor (setA 0.4 (toRgba lightpink))
    borderColor red
    display block
  ".lose" ? ".banner" # ":before" ? do
    content (stringContent "Loser: ")

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
