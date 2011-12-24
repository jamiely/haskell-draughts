module Checkers ( Game ) where

import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Map (Map)

type Player = Marker

type Size = (Int, Int) -- Width, Height
type Position = (Int, Int) -- row, col
type PositionMap = Map Position Marker

data Game = Game GameState [Player] deriving (Show)
data Board = Board Size PositionMap deriving (Show, Eq) -- Width, Height, Markers
data GameState = GameState Board Player deriving (Show, Eq) -- State, Board, Current Player
data Marker = None | Black | Red | King Marker deriving (Show, Eq)

getDefaultGame :: Game
getDefaultGame = Game startingState [Black, Red] where
  startingState = GameState emptyBoard Black
  emptyBoard = getBoard (10, 10)

markerAt :: PositionMap -> Position -> Marker
markerAt orig pos = maybe None id (Map.lookup pos orig)

swap :: PositionMap -> Position -> Position -> PositionMap 
swap orig from to = new where
  mrk = markerAt orig
  new = Map.insert from (mrk to) $ Map.insert to (mrk from) orig

swapMarkers :: Board -> Position -> Position -> Board
swapMarkers (Board sz posMap) from to = Board sz $ swap posMap from to

testSwap :: Test
testSwap = "swap" ~: TestList [
  swap orig (1,2) (2,1) ~?= Map.fromList boardList
  ] where
  black = posMapPair Black
  p = posMapPair None
  orig = Map.fromList [p 1 1, p 1 2, p 1 3, black 2 1, p 2 2, p 2 3]
  boardList = [p 1 1, black 1 2, p 1 3, p 2 1, p 2 2, p 2 3]

-- | Returns a list of Markers representing a board
getBoard :: Size -> Board
getBoard sz = Board sz $ getBoardPositionMap sz

getBoardPositionMap :: Size -> PositionMap 
getBoardPositionMap (width, height) = Map.fromList $ zip positions (repeat None) where
  positions = [(row, col) | row <- [1..height], col <- [1..width]]

testGetBoardPositionMap :: Test
testGetBoardPositionMap = "getBoardPositionMap" ~: TestList [
  getBoardPositionMap (3,2) ~?= Map.fromList boardList
  ] where
  p = posMapPair None
  boardList = [p 1 1, p 1 2, p 1 3, p 2 1, p 2 2, p 2 3]

testGetBoard :: Test
testGetBoard = "getBoard" ~: TestList [
  markers (getBoard (3, 2)) ~?= getBoardPositionMap (3, 2)
  ] 

posMapPair :: Marker -> Int -> Int -> (Position, Marker)
posMapPair mark r c = ((r,c), mark)

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
  newMarks = Map.insert pos mark ms

testUpdateBoard :: Test
testUpdateBoard = "updateBoard" ~: TestList [
  markers origBoard ~?= Map.fromList boardList,
  updateBoard origBoard (1,2) Black ~?= 
    Board sz (Map.fromList newBoardList)
  ] where
  sz = (3,2)
  origBoard = getBoard sz
  p = posMapPair None
  boardList = [p 1 1, p 1 2, p 1 3, p 2 1, p 2 2, p 2 3]
  newBoardList = [p 1 1, posMapPair Black 1 2, p 1 3, p 2 1, p 2 2, p 2 3]

emptyPositions :: Board -> [Position]
emptyPositions (Board _ posMap) = Map.keys $ Map.filter (== None) posMap

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
    testGetBoardPositionMap,
    testGetBoard,
    testSwap,
    testUpdateBoard,
    testUpdateState
    ])
  return ()

