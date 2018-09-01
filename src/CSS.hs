{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay

import Data.ByteString (ByteString)

css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  pure ()
