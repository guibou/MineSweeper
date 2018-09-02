{-# LANGUAGE TupleSections #-}
module MineSweeper
  ( newGame
  , Size(..)
  , MineAction(..)
  , Coord(..)
  , FieldState
  , Status(..)
  , HiddenStatus(..)
  , StatusModifier(..)
  , getGameStatus
  , play
  , fieldSize
  , allCells
  , getFieldStatus
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
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

data HiddenStatus = Unknown | Flagged
  deriving (Show, Eq)

data FieldState = FieldState (Map Coord HiddenStatus) Life Field
  deriving Show

data Status = Bomb | SafeArea Int
  deriving (Show, Eq)

data Life = Alive | Dead
  deriving (Show)

data MineAction = Flag | Open
  deriving (Show)

data GameStatus
  = Win
  | Lose
  | Current Int
  deriving (Show)

getGameStatus :: FieldState -> GameStatus
getGameStatus (FieldState _ Dead _) = Lose
getGameStatus (FieldState visibility Alive (Field _ mines))
  | nbHidden == nbMines = Win
  | otherwise = Current nbHidden

  where
    nbMines = Set.size mines
    nbHidden = Map.size visibility

newGame :: Int -> Size -> Int -> FieldState
newGame seed size mineCount = FieldState popMap Alive . Field size $ randomPickN pop mineCount (mkStdGen seed)
  where
    pop = universe size
    popMap = Map.fromSet (const Unknown) pop

fieldSize :: FieldState -> Size
fieldSize (FieldState _ _ (Field size _)) = size

_display :: FieldState -> IO ()
_display fs@(FieldState _ _ _) = do
  case fieldStatus fs of
    Dead -> putStrLn "DEAD"
    _ -> pure ()
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

fieldStatus :: FieldState -> Life
fieldStatus (FieldState _ life _) = life

data StatusModifier
  = Visible
  | Hidden HiddenStatus
  deriving (Show, Eq)

getFieldStatus :: Coord -> FieldState -> (StatusModifier, Status)
getFieldStatus c (FieldState visibility _ f) = (modifier, getStatus c f)
  where
    modifier = maybe Visible Hidden (Map.lookup c visibility)

getStatus :: Coord -> Field -> Status
getStatus coord (Field _ bombs)
  | coord `Set.member` bombs = Bomb
  | otherwise = SafeArea (count (`Set.member` bombs) (border coord))
      
play :: (Coord, MineAction) -> FieldState -> FieldState
play _ fs@(FieldState _ Dead _) = fs
play (c, action) fs@(FieldState visibility Alive field) = case action of
  Open
   | Just Flagged <- Map.lookup c visibility -> fs
   | Nothing <- Map.lookup c visibility -> fs -- already opened
   | otherwise -> case getStatus c field of
     Bomb -> newField Dead
     SafeArea 0 -> foldl (\f coord -> play (coord, Open) f) (newField Alive) (border c)
     SafeArea _ -> newField Alive
   where
       newField l = FieldState (Map.delete c visibility) l field
  Flag -> FieldState (Map.alter fAlter c visibility) Alive field
    where fAlter Nothing = Nothing
          fAlter (Just Unknown) = Just Flagged
          fAlter (Just Flagged) = Just Unknown
