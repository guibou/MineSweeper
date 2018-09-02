{-# OPTIONS -Wall #-}
module MineSweeper
  ( newGame
  , Size(..)
  , Coord(..)
  , FieldState
  , Status(..)
  , play
  , fieldSize
  , allCells
  , debugStatus
  , getFieldStatus
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (for_)
import Control.Monad (guard)
import System.Random

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

data Status = Bomb | SafeArea Int
  deriving (Show, Eq)

data Life = Alive | Dead
  deriving (Show)

newGame :: Int -> Size -> Int -> FieldState
newGame seed size mineCount = FieldState pop Alive . Field size $ randomPickN pop mineCount (mkStdGen seed)
  where pop = universe size

isVisible :: Coord -> FieldState -> Bool
isVisible c (FieldState hidden _ _) = not $ c `Set.member` hidden


fieldSize :: FieldState -> Size
fieldSize (FieldState _ _ (Field size _)) = size

display :: FieldState -> IO ()
display fs@(FieldState _ _ field) = do
  case fieldStatus fs of
    Dead -> putStrLn "DEAD"
    _ -> pure ()
  for_ (allCells (fieldSize fs)) $ \line -> do
    for_ line $ \coord -> do
      putStr (debugStatus fs coord)
    putChar '\n'

debugStatus field coord = paren visible (charStatus coord)
  where
    (visible, status) = getFieldStatus coord field

    paren True c = [' ', c, ' ']
    paren False c = ['[', c, ']']

    charStatus coord = case status of
      Bomb -> '*'
      SafeArea 0 -> '_'
      SafeArea i -> head (show i)

fieldStatus :: FieldState -> Life
fieldStatus (FieldState _ life _) = life

getFieldStatus :: Coord -> FieldState -> (Bool, Status)
getFieldStatus c field@(FieldState _ _ f) = (isVisible c field, getStatus c f)

getStatus :: Coord -> Field -> Status
getStatus coord (Field _ bombs)
  | coord `Set.member` bombs = Bomb
  | otherwise = SafeArea (count (`Set.member` bombs) (border coord))
      
play :: Coord -> FieldState -> FieldState
play c fs@(FieldState hiddens life field)
  | not $ c `Set.member` hiddens = fs
  | otherwise = case getStatus c field of
    Bomb -> newField Dead
    SafeArea 0 -> foldl (flip play) (newField life) (border c)
    SafeArea _ -> newField life
  where
      newField l = FieldState (Set.delete c hiddens) l field
