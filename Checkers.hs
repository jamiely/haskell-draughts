module Checkers ( Game ) where

import Test.HUnit
import Data.Map

type Player = Marker

type Size = (Int, Int) -- Width, Height
type Position = (Int, Int) -- row, col
type PositionMap = Map Position Marker

data Game = Game GameState [Player] deriving (Show)
data Board = Board Size PositionMap deriving (Show, Eq) -- Width, Height, Markers
data GameState = GameState Board Player deriving (Show, Eq) -- State, Board, Current Player
data Marker = None | Black | Red | King Marker deriving (Show, Eq)

-- | Returns a list of Markers representing a board
getBoard :: Size -> Board
getBoard sz@(width, height) = board where
  board = Board sz markers
  positions = [(row, col) | row <- [1..height], col <- [1..width]]
  markers = fromList $ zip positions (repeat None) :: PositionMap

testGetBoard :: Test
testGetBoard = "getBoard" ~: TestList [
  markers (getBoard (3, 2)) ~?= fromList boardList
  ] where
  p :: Int -> Int -> (Position, Marker)
  p r c = ((r,c), None)
  boardList = [p 1 1, p 1 2, p 1 3, p 2 1, p 2 2, p 2 3]

size :: Board -> Size
size (Board s _) = s

-- | Retrieve the markers of a board
markers :: Board -> PositionMap
markers (Board _ ms) = ms

-- | Update the markers associated with a board
swapMarkers :: Board -> PositionMap -> Board
swapMarkers (Board sz _) ms = Board sz ms

-- | Use to update the mark on the board
updateBoard :: Board -> Position -> Marker -> Board
updateBoard board pos@(row, col) mark = newBoard where
  ms = markers board
  newBoard = swapMarkers board newMarks
  newMarks = insert pos mark ms

testUpdateBoard :: Test
testUpdateBoard = "updateBoard" ~: TestList [
  markers origBoard ~?= fromList boardList,
  updateBoard origBoard (1,2) Black ~?= 
    Board sz (fromList newBoardList)
  ] where
  sz = (3,2)
  origBoard = getBoard sz
  p :: Int -> Int -> (Position, Marker)
  p r c = ((r,c), None)
  boardList = [p 1 1, p 1 2, p 1 3, p 2 1, p 2 2, p 2 3]
  newBoardList = [p 1 1, ((1,2), Black), p 1 3, p 2 1, p 2 2, p 2 3]


-- | Updates the game state by making a move at the 
-- passed position
updateState :: GameState -> Position -> GameState
updateState orig@(GameState origBoard origPlayer) pos = 
  GameState newBoard next where
  newBoard = updateBoard origBoard pos origPlayer
  next = case origPlayer of 
    Black -> Red
    _     -> Black

testUpdateState :: Test
testUpdateState = "updateState" ~: TestList [
  updateState origState pos ~?= newState
  ] where
  origState = GameState origBoard Black
  newState = GameState newBoard Red
  pos = (1, 2)

  sz = (3, 2)
  origBoard = getBoard sz
  newBoard = updateBoard origBoard pos Black

test :: IO ()
test = do
  runTestTT (TestList [
    testGetBoard,
    testUpdateBoard,
    testUpdateState
    ])
  return ()

