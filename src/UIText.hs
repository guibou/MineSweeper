module UIText where

import MineSweeper
import Data.Foldable (for_)

display :: GameState -> IO ()
display fs = do
  print (gameResult fs)
  for_ (allCells (fieldSize fs)) $ \line -> do
    for_ line $ \coord -> do
      putStr (debugStatus fs coord)
    putChar '\n'

debugStatus :: GameState -> Coord -> [Char]
debugStatus field coord = paren visible charStatus
  where
    (visible, status) = caseStatus coord field

    paren Visible c = [' ', c, ' ']
    paren (Hidden NotFlagged) c = ['[', c, ']']
    paren (Hidden Flagged) c = ['{', c, '}']

    charStatus = case status of
      Bomb -> '*'
      SafeArea 0 -> '_'
      SafeArea i -> head (show i)
