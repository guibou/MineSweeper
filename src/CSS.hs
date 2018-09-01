{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay

import Data.ByteString (ByteString)

css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  table ? do
    width (pct 100)
    height (pct 100)
    borderCollapse collapse
    "table-layout" -: "fixed"
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

    ".safe1" ? color blue
    ".safe2" ? color green
    ".safe3" ? color red
    ".safe4" ? color orange
    ".safe5" ? color yellow
    ".safe6" ? color purple
    ".safe7" ? color pink
    ".safe8" ? color black
