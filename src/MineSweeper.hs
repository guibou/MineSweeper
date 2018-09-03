-- | All the logic of the MineSweeper game
{-# LANGUAGE TupleSections #-}
module MineSweeper
  (
    -- * Coord / Size
    Size(..)
  , Coord(..)
  , allCells
  -- * Game
  , newGame
  , GameState
  , fieldSize
  -- * Game Status
  , CaseContent(..)
  , Flagged(..)
  , Visibility(..)
  , GameResult(..)
  , gameResult
  , caseStatus
  -- * Game Actions
  , MineAction(..)
  , play
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
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

-- | This is a game board
data Field = Field Size (Set Coord) -- ^ Size and a set of mines
  deriving Show

-- * Game Status

-- | An hidden case can be flagged or not
data Flagged = NotFlagged | Flagged
  deriving (Show, Eq)

-- | Opaque type. Stores a Mine game
data GameState = GameState (Map Coord Flagged) Life Field
  deriving Show

-- | What's inside a case
data CaseContent
  = Bomb -- ^ A Bomb
  | SafeArea Int -- ^ No Bomb, there are 'Int' Bomb in the border
  deriving (Show, Eq)

-- | Current game status. TODO: merge with GameResult
data Life = Alive | Dead
  deriving (Show)

-- | State of the current game
data GameResult
  = Win -- ^ This game is done and won
  | Lose -- ^ Done and lost
  | Current Int -- ^ Still running
  deriving (Show)

-- | Information of visibility
data Visibility
  = Visible -- ^ The case is Visible, i.e. the player know what's under
  | Hidden Flagged -- ^ The case is hidden. the player may have set a flog on it
  deriving (Show, Eq)

-- * Game Action

-- | An action on the game
data MineAction
  = Flag -- ^ Set a flag
  | Reveal -- ^ Reveal a case
  deriving (Show)

-- | Tells if we are still playing or if the game is done
gameResult :: GameState -> GameResult
gameResult (GameState _ Dead _) = Lose
gameResult (GameState visibility Alive (Field _ mines))
  | nbHidden == nbMines = Win
  | otherwise = Current nbHidden

  where
    nbMines = Set.size mines
    nbHidden = Map.size visibility

-- | Create a new game
newGame
  :: Int -- ^ Random seed
  -> Size -- ^ New size
  -> Int -- ^ Number of mines
  -> GameState
newGame seed size mineCount = GameState popMap Alive . Field size $ randomPickN pop mineCount (mkStdGen seed)
  where
    pop = universe size
    popMap = Map.fromSet (const NotFlagged) pop

-- | Returns the 'Size' of the game
fieldSize :: GameState -> Size
fieldSize (GameState _ _ (Field size _)) = size

-- | Returns what's inside the case and what the player can see
caseStatus :: Coord -> GameState -> (Visibility, CaseContent)
caseStatus c (GameState visibility _ f) = (modifier, getStatus c f)
  where
    modifier = maybe Visible Hidden (Map.lookup c visibility)

-- | What's in a case
getStatus :: Coord -> Field -> CaseContent
getStatus coord (Field _ bombs)
  | coord `Set.member` bombs = Bomb
  | otherwise = SafeArea (count (`Set.member` bombs) (border coord))


-- | Play a game action
-- An action can 'Reveal' or 'Flag' a 'Coord'
-- - If the action 'Flag', it will toggle the 'Flagged' status only of an 'Hidden' case
-- - If the action 'Reveal', it can only reveal a not 'Flagged' case
-- - If the game is 'Win' or 'Lose', action have no effect
play
  :: MineAction
  -> Coord
  -> GameState
  -> GameState
play _ _ fs@(GameState _ Dead _) = fs
play action c fs@(GameState visibility Alive field) = case action of
  Reveal
   | Just Flagged <- Map.lookup c visibility -> fs
   | Nothing <- Map.lookup c visibility -> fs -- already opened
   | otherwise -> case getStatus c field of
     Bomb -> newField Dead
     SafeArea 0 -> foldl (\f coord -> play Reveal coord f) (newField Alive) (border c)
     SafeArea _ -> newField Alive
   where
       newField l = GameState (Map.delete c visibility) l field
  Flag -> GameState (Map.alter fAlter c visibility) Alive field
    where fAlter Nothing = Nothing
          fAlter (Just NotFlagged) = Just Flagged
          fAlter (Just Flagged) = Just NotFlagged
