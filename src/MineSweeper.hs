{-# OPTIONS -Wall #-}
module MineSweeper
  ( newGame
  , Size(..)
  , play
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (for_)
import Control.Monad (guard)

import Utils

-- * Coord / Size

-- | A position in a 2D grid
data Coord = Coord Int Int
  deriving (Show, Ord, Eq)

-- | Size of the 2D grid
data Size = Size Int Int
  deriving (Show)

-- | All cells in the 2D grid
allCells :: Size -> [[Coord]]
allCells (Size w h) = do
  y <- [0..(w-1)]
  pure $ do
    x <- [0..(h-1)]
    pure $ Coord x y

-- | Cells around another cell
border :: Coord -> [Coord]
border (Coord x y) = do
  dx <- [-1..1]
  dy <- [-1..1]

  guard $ (dx, dy) /= (0, 0)
  pure (Coord (x + dx) (y + dy))

-- | All cells in a 2D Grid, set version
universe :: Size -> Set Coord
universe = Set.fromList . mconcat . allCells

-- * Game

data Field = Field Size (Set Coord)
  deriving Show

data FieldState = FieldState (Set Coord) Life Field
  deriving Show

data Status = Bomb | Safe Int
  deriving Show

data Life = Alive | Dead
  deriving (Show)

newGame :: Size -> Int -> IO FieldState
newGame size mineCount = (FieldState pop Alive . Field size) <$> randomPickN pop mineCount
  where pop = universe size

isVisible :: Coord -> FieldState -> Bool
isVisible c (FieldState hidden _ _) = not $ c `Set.member` hidden


display :: FieldState -> IO ()
display fs@(FieldState _ _ field@(Field size _)) = do
  case fieldStatus fs of
    Dead -> putStrLn "DEAD"
    _ -> pure ()
  for_ (allCells size) $ \line -> do
    for_ line $ \coord -> do
      putStr (paren (isVisible coord fs) (charStatus coord))
    putChar '\n'
  where
    paren True c = [' ', c, ' ']
    paren False c = ['[', c, ']']

    charStatus coord = case getStatus coord field of
      Bomb -> '*'
      Safe 0 -> '_'
      Safe i -> head (show i)

fieldStatus :: FieldState -> Life
fieldStatus (FieldState _ life _) = life

getStatus :: Coord -> Field -> Status
getStatus coord (Field _ bombs)
  | coord `Set.member` bombs = Bomb
  | otherwise = Safe (count (`Set.member` bombs) (border coord))
      
play :: Coord -> FieldState -> FieldState
play c fs@(FieldState hiddens life field)
  | not $ c `Set.member` hiddens = fs
  | otherwise = case getStatus c field of
    Bomb -> newField Dead
    Safe 0 -> foldl (flip play) (newField life) (border c)
    Safe _ -> newField life
  where
      newField l = FieldState (Set.delete c hiddens) l field
