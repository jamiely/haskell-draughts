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

isInBounds :: Board -> Position -> Bool
isInBounds board pos = posX >= 0 && posX < boardX && posY >= 0 && posY < boardY where
  (boardX, boardY) = size board
  (posX, posY) = pos

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

-- | Given a Board and a position on a board, returns the valid
-- moves for the marker. Markers may move "forward" diagonally,
-- unless they are King-ed, in which case they may move in any
-- direction. Markers may jump markers of the opposing player to move
-- two spaces diagonally. 
boardMoves :: Board -> Position -> [Position]
boardMoves board src = boardWalk board src ++ boardJump board src

-- | Given a Board and a position on the board, returns the valid
-- walks for the marker, that is, the places where it may move a single
-- position without jumping another piece.
boardWalk :: Board -> Position -> [Position]
boardWalk board src = filter (isInBounds board) (possibleMoves marker) where
  marker = markerAt (markers board) src
  (x, y) = src
  possibleMoves (King _) = possibleMoves Black ++ possibleMoves Red
  possibleMoves Black = [(x-1,y+1), (x+1,y+1)]
  possibleMoves Red = [(x-1,y-1), (x+1,y-1)]
  possibleMoves _ = []

-- | Given a Board and a position on the board, returns the valid
-- jumps for the marker
boardJump board src = filter (isInBounds board) (possibleMoves marker) where
  marker = markerAt (markers board) src
  (x, y) = src
  possibleMoves (King color) = jumpsTowardsRedHome color ++ jumpsTowardsBlackHome color
  possibleMoves Black = jumpsTowardsRedHome Black
  possibleMoves Red = jumpsTowardsBlackHome Red
  possibleMoves _   = []

  jumpsTowardsRedHome Black = 
  possibleDiagonals = diagonalN 2 src
  possibleJumpedPieces = diagonals
  isTowardsBlack (_, sy) (_, dy) = dy < sy
  isTowardsRed (_, sy) (_, dy) = dy > sy

-- | Returns all the positions which are the passed param away from the
-- passed position.
diagonalN :: Int -> Position -> [Position]
diagonalN d (r, c) = [(r+d, c+d), (r-d, c-d), (r+d, c-d), (r-d, c+d)]

testDiagonalN :: Test
testDiagonalN = "Test diagonalN" ~: TestList [
  diagonalN 1 (0, 0) ~?= [(1,1), (-1, -1), (1, -1), (-1, 1)]
  ]

-- | Returns all the positions which are 1 diagonal away from the passed
-- position.
diagonals :: Position -> [Position]
diagonals = diagonalN 1

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

