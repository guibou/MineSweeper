module UIText where

import MineSweeper
import Data.Foldable (for_)

display :: FieldState -> IO ()
display fs = do
  print (getGameStatus fs)
  for_ (allCells (fieldSize fs)) $ \line -> do
    for_ line $ \coord -> do
      putStr (debugStatus fs coord)
    putChar '\n'

debugStatus :: FieldState -> Coord -> [Char]
debugStatus field coord = paren visible charStatus
  where
    (visible, status) = getFieldStatus coord field

    paren Visible c = [' ', c, ' ']
    paren (Hidden Unknown) c = ['[', c, ']']
    paren (Hidden Flagged) c = ['{', c, '}']

    charStatus = case status of
      Bomb -> '*'
      SafeArea 0 -> '_'
      SafeArea i -> head (show i)
