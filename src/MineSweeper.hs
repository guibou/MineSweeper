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
data GameState = GameState (Map Coord Flagged) Field
  deriving Show

-- | What's inside a case
data CaseContent
  = Bomb -- ^ A Bomb
  | SafeArea Int -- ^ No Bomb, there are 'Int' Bomb in the border
  deriving (Show, Eq)

-- | State of the current game
data GameResult
  = Win -- ^ This game is done and won
  | Lose -- ^ Done and lost
  | Playing -- ^ Still playing
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
gameResult (GameState visibility (Field _ mines))
  | not (null (mines `Set.difference` (Map.keysSet visibility))) = Lose -- A mine is visible
  | nbHidden == nbMines = Win -- all mines are hidden / flagged
  | otherwise = Playing
  where
    nbMines = Set.size mines
    nbHidden = Map.size visibility

-- | Create a new game
newGame
  :: Int -- ^ Random seed
  -> Size -- ^ New size
  -> Int -- ^ Number of mines
  -> GameState
newGame seed size mineCount = GameState popMap . Field size $ randomPickN pop mineCount (mkStdGen seed)
  where
    pop = universe size
    popMap = Map.fromSet (const NotFlagged) pop

-- | Returns the 'Size' of the game
fieldSize :: GameState -> Size
fieldSize (GameState _ (Field size _)) = size

-- | Returns what's inside the case and what the player can see
caseStatus :: Coord -> GameState -> (Visibility, CaseContent)
caseStatus c (GameState visibility f) = (modifier, getStatus c f)
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
-- - If the action 'Reveal', it reveals a not 'Flagged' case or remove the 'Flagged' status
-- - If the game is 'Done', it won't change anything
play
  :: MineAction
  -> Coord
  -> GameState
  -> GameState
play action c fs@(GameState visibility field)
  | Playing <- gameResult fs = case action of
  Reveal
   | Just Flagged <- Map.lookup c visibility -> play Flag c fs -- Reveal a 'Flagged': removes the Flag status
   | Nothing <- Map.lookup c visibility -> fs -- already opened
   | otherwise -> GameState (reveal c visibility field) field -- We can cascade reveal this case
  Flag -> GameState (Map.alter (fmap flipFlag) c visibility) field -- toggle the flag status if any
  | otherwise = fs -- If not playing, don't change anything

-- | Unconditionally 'Reveal' a case and its border
reveal :: Coord -> Map Coord a -> Field -> Map Coord a
reveal c visibility field
  | Nothing <- Map.lookup c visibility = visibility -- already revealed
  | otherwise = case getStatus c field of
    SafeArea 0 -> foldl (\v coord -> reveal coord v field) newVisibility (border c) -- cascade reveal
    _ -> newVisibility
   where
       newVisibility = Map.delete c visibility

-- | Toggle the flagged status
flipFlag :: Flagged -> Flagged
flipFlag NotFlagged = Flagged
flipFlag Flagged = NotFlagged
