{-# LANGUAGE OverloadedStrings #-}
module UIReflex.CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay
import Clay.Stylesheet

import Data.ByteString (ByteString)

headerSize = 5
gridSizeW = 17
gridSizeH = 9
gridSizeM = Prelude.max gridSizeW gridSizeH

css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  star ? do
    padding (px 0) 0 0 0
    margin (px 0) 0 0 0
    borderWidth (px 0)

  body ? do
    margin (pct 0) 0 0 0

  ".header" ? do
    display flex
    height (vh (headerSize - 0.95))

    ".mineCount" ? do
      fontColor red
    ".timer" ? do
      animationName "expand"
      animationDuration (sec 0.5)

      keyframes "expand" [(0, Clay.opacity 0)]

  -- custom override for all winners
  table # ".win" ? do
    padding auto auto auto auto
    td # ".unknown" ? span # ":after" ? do -- not flagged bomb
      color white
      content (stringContent "B")

  -- custom override for losers
  table # ".lose" ? do
    -- Wrong bomb
    -- Wrong flag
    -- Bomb
    -- Case OK
    pure ()

  table ? do
    borderCollapse collapse
    borderWidth (px 0)

    td # ".flagged" ? span # ":after" ? do
      content (stringContent "F")

    td ? do
      boxSizing borderBox
      width (vw (100 / gridSizeW))
      height (vh ((100 - headerSize) / gridSizeH))
      fontFamily [] [monospace]
      fontSize (em 2) -- TODO: find a real way to scale that, using images ?
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

  table # ".playing" ? td # ".visible" ? displays
  table # ".win" ? td # ".visible" ? displays
  table # ".lose" ? td ? displays

displays :: StyleM ()
displays = do
      span # ":after" ? do
        content (stringContent "B") -- No number, you are a bomb
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
