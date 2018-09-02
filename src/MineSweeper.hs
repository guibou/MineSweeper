{-# LANGUAGE TupleSections #-}
module MineSweeper
  ( newGame
  , Size(..)
  , MineAction(..)
  , Coord(..)
  , FieldState
  , Status(..)
  , StatusModifier(..)
  , getGameStatus
  , play
  , fieldSize
  , allCells
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

-- TODO: two set sucks
-- I need to refactor with only one Map of Visible | Hidden | Flag
data FieldState = FieldState (Set Coord) (Set Coord) Life Field
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
  | Current Int Int Int
  deriving (Show)

getGameStatus :: FieldState -> GameStatus
getGameStatus (FieldState _ _ Dead _) = Lose
getGameStatus (FieldState hidden flags Alive (Field _ mines))
  | nbOpenableCases == (nbMines - nbFlags) = Win
  | otherwise = Current (nbMines - nbFlags) nbFlags nbOpenableCases

  where
    nbMines = Set.size mines
    nbFlags = Set.size flags
    nbHidden = Set.size hidden

    nbOpenableCases = nbHidden - nbFlags

newGame :: Int -> Size -> Int -> FieldState
newGame seed size mineCount = FieldState pop (Set.empty) Alive . Field size $ randomPickN pop mineCount (mkStdGen seed)
  where pop = universe size

isVisible :: Coord -> FieldState -> Bool
isVisible c (FieldState hidden _ _ _) = not $ c `Set.member` hidden

isFlagged :: Coord -> FieldState -> Bool
isFlagged c (FieldState _ flags _ _) = c `Set.member` flags

fieldSize :: FieldState -> Size
fieldSize (FieldState _ _ _ (Field size _)) = size

_display :: FieldState -> IO ()
_display fs@(FieldState _ _ _ _) = do
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
    paren Hidden c = ['[', c, ']']
    paren Flagged c = ['{', c, '}']

    charStatus = case status of
      Bomb -> '*'
      SafeArea 0 -> '_'
      SafeArea i -> head (show i)

fieldStatus :: FieldState -> Life
fieldStatus (FieldState _ _ life _) = life

data StatusModifier
  = Visible
  | Flagged
  | Hidden
  deriving (Show, Eq)

getFieldStatus :: Coord -> FieldState -> (StatusModifier, Status)
getFieldStatus c field@(FieldState _ _ _ f) = (modifier, getStatus c f)
  where
    modifier
     | isVisible c field = Visible
     | isFlagged c field = Flagged
     | otherwise = Hidden

getStatus :: Coord -> Field -> Status
getStatus coord (Field _ bombs)
  | coord `Set.member` bombs = Bomb
  | otherwise = SafeArea (count (`Set.member` bombs) (border coord))
      
play :: (Coord, MineAction) -> FieldState -> FieldState
play _ fs@(FieldState _ _ Dead _) = fs
play (c, action) fs@(FieldState hiddens flags Alive field) = case action of
  Open
   | c `Set.member` flags -> fs -- cannot open if flagged
   | not $ c `Set.member` hiddens -> fs -- already opened
   | otherwise -> case getStatus c field of
     Bomb -> newField Dead
     SafeArea 0 -> foldl (\f coord -> play (coord, Open) f) (newField Alive) (border c)
     SafeArea _ -> newField Alive
   where
       newField l = FieldState (Set.delete c hiddens) flags l field
  Flag
   | not $ c `Set.member` hiddens -> fs -- cannot flag if open
   | otherwise -> FieldState hiddens (toggleInSet c flags) Alive field

toggleInSet :: Ord t => t -> Set t -> Set t
toggleInSet v set
  | v `Set.member` set = Set.delete v set
  | otherwise = Set.insert v set
