module Checkers ( Game, testAll ) where

import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Map (Map)

type Size = (Int, Int) -- Width, Height
type Position = (Int, Int) -- row, col
type PositionMap = Map Position Marker

data Player = Player Marker deriving (Show, Eq)
data Game = Game GameState [Player] deriving (Show)
data Board = Board Size PositionMap deriving (Eq) -- Width, Height, Markers
data GameState = GameState Board Player deriving (Show, Eq) -- State, Board, Current Player
data Marker = None | Black | Red | King Marker deriving (Show, Eq)

instance Show Board where
  show board@(Board (width, height) posMap) = join rows where
    rows = chunk width markers
    markers = concat $ map (\k -> showMarker $ posMap Map.! k) positions
    positions = boardPositions board
    chunk _ [] = []
    chunk n xs = y1 : chunk n y2 where
      (y1, y2) = splitAt n xs
    join = foldr (\s ss -> ss ++ ('\n' : s)) ""

showMarker :: Marker -> String
showMarker marker = case marker of 
                      None       -> "."
                      Black      -> "b"
                      Red        -> "r"
                      King Black -> "B"
                      King Red   -> "R"
                      _          -> "?"

boardPositions :: Board -> [Position]
boardPositions (Board (width, height) _) = [(c, r) | r <- [1..height], c <- [1..width]]

getDefaultGame :: Game
getDefaultGame = Game startingState [Player Black, Player Red] where
  startingState = GameState getDefaultBoard $ Player Black

getDefaultBoard :: Board
getDefaultBoard = filledBoard where
  emptyBoard = getBoard (8, 8)
  filledBoard = updateBoardPositions filledRedBoard blackPositions Black
  filledRedBoard = updateBoardPositions emptyBoard redPositions Red
  blackPositions = genPos [1..3]
  redPositions = genPos [6..8]
  genPos rows = [(c, d) | c <- [1..8], d <- rows, or [and [even c, even d], and [odd c, odd d]] ]

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

updateBoardPositions :: Board -> [Position] -> Marker -> Board
updateBoardPositions board positions marker = updatedBoard where
  updatedBoard = foldr updatePos board positions
  updatePos pos board = updateBoard board pos marker

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

hasWon :: Board -> Marker -> Bool
hasWon _ None = False
hasWon board (King marker) = hasWon board marker
hasWon (Board _ posMap) marker = not otherMarkerExists where
  otherMarkerExists = any (eqMarker otherMarker) elems
  otherMarker = toggleColor $ case marker of
                  King m -> m
                  m      -> m
  eqMarker m a = a == m || a == King m
  elems = Map.elems posMap

testHasWon :: Test
testHasWon = "Test has won" ~: TestList [
  hasWon b1 Red ~?= True,
  hasWon b1 Black ~?= False,
  hasWon b2a Red ~?= False,
  hasWon b2b Red ~?= False,
  hasWon b2c Red ~?= True
  ] where
  b0 = getBoard (2,2)
  b1 = foldr (\pos bp -> updateBoard bp pos Red) b0 (emptyPositions b0)
  b2a = updateBoard b1 (1,1) Black
  b2b = updateBoard b1 (1,1) (King Black)
  b2c = updateBoard b1 (1,1) (King Red)

-- | Updates the game state by making a move at the 
-- passed position
updateState :: GameState -> Position -> GameState
updateState orig@(GameState origBoard origPlayer) pos = 
  GameState newBoard next where
  newBoard = updateBoard origBoard pos (case origPlayer of Player m -> m)
  next = case origPlayer of 
    Player Black -> Player Red
    _ -> Player Black

testUpdateState :: Test
testUpdateState = "updateState" ~: TestList [
  updateState origState pos ~?= newState
  ] where
  origState = GameState origBoard $ Player Black
  newState = GameState newBoard $ Player Red
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

testBoardMoves :: Test
testBoardMoves = "Test board jumps" ~: TestList [
  boardMoves board2 startPos ~?= [(1,3),(3,3)],
  boardMoves board3 startPos ~?= [(1,3),(4,4)],
  boardMoves board4 (2,4) ~?= [(1,3),(4,2)]
  ] where
  board1 = getBoard(5,5)
  board2 = updateBoard board1 (2,2) Black
  board3 = updateBoard board2 (3,3) Red
  board4 = updateBoard board3 (2,4) (King Black)
  -- 4..K..
  -- 3...R.
  -- 2..B..
  -- 1.....
  -- 0.....
  --  01234
  startPos = (2,2)

-- | Given a Board and a position on the board, returns the valid
-- walks for the marker, that is, the places where it may move a single
-- position without jumping another piece.
boardWalk :: Board -> Position -> [Position]
boardWalk board src = filter (inBoundsAndEmpty board) (possibleMoves marker) where
  marker = markerAt (markers board) src
  (x, y) = src
  possibleMoves (King _) = possibleMoves Black ++ possibleMoves Red
  possibleMoves Black = [(x-1,y+1), (x+1,y+1)]
  possibleMoves Red = [(x-1,y-1), (x+1,y-1)]
  possibleMoves _ = []

isTowards :: Marker -> Position -> Position -> Bool
isTowards Red (_, sy) (_, dy)   = dy > sy
isTowards Black (_, sy) (_, dy) = dy < sy

inBoundsAndEmpty board pos = isInBounds board pos && boardEmptyAt board pos

boardEmptyAt :: Board -> Position -> Bool
boardEmptyAt board pos = m == None where
  m = markerAt (markers board) pos

-- | Given a Board and a position on the board, returns the valid
-- jumps for the marker
boardJump :: Board -> Position -> [Position]
boardJump board src = jumps where
  jumps = filter (inBoundsAndEmpty board) (possibleMoves marker)
  marker = markerAt (markers board) src

  possibleMoves :: Marker -> [Position]
  possibleMoves Black        = possibleMovesColor Black
  possibleMoves Red          = possibleMovesColor Red
  possibleMoves (King color) = possibleJumps
  possibleMoves _            = []

  possibleMovesColor :: Marker -> [Position]
  possibleMovesColor color = filter towards moves where
    towards = isTowards (toggleColor color) src
    moves = possibleJumps

  -- Tuples of a jumped position and the jump destination
  possibleJumps = map (\(_, a, _) -> a) jumps where 
    jumps = filter canJump possibleJumpTuples
    possibleJumpTuples = zip3 diag1 diag2 piecesAtDiagonals
    diag1 = diagonals src
    diag2 = diagonalN 2 src
    piecesAtDiagonals = map (markerAt (markers board)) diag1
    -- A piece can jump to the position if it jumps over a 
    -- piece of the opposing color
    canJump :: (Position, Position, Marker) -> Bool
    canJump (_, _, m) = markerMayJumpMarker marker m

testBoardJump :: Test
testBoardJump = "Test board jumps" ~: TestList [
  boardJump board3 startPos ~?= [(4,4)], 
  boardJump board4 startPos ~?= [(4,4)],
  boardJump board5a startPos ~?= [(4,4), (0,4)], 
  boardJump board5b startPos ~?= [(4,4)],
  boardJump board6 startPos ~?= [(4,4)]
  ] where
  board1 = getBoard(5, 5) 
  board2 = updateBoard board1 (2, 2) Black
  board3 = updateBoard board2 (3, 3) Red
  board4 = updateBoard board3 (1, 1) Red
  board5a = updateBoard board4 (1, 3) Red
  board5b = updateBoard board4 (1, 3) Black
  board6 = updateBoard board5a (0, 4) Red
  startPos = (2, 2) -- center of board

toggleColor :: Marker -> Marker
toggleColor Black        = Red
toggleColor Red          = Black
toggleColor (King color) = King $ toggleColor color
toggleColor c            = c

markerMayJumpMarker :: Marker -> Marker -> Bool
markerMayJumpMarker None _ = False
markerMayJumpMarker marker (King m) = m /= None && markerMayJumpMarker marker m
markerMayJumpMarker marker Red = marker /= Red
markerMayJumpMarker marker Black = marker /= Black
markerMayJumpMarker _ _ = False

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

testAll :: IO ()
testAll = do
  runTestTT (TestList [
    testGetBoardPositionMap,
    testGetBoard,
    testSwap,
    testUpdateBoard,
    testUpdateState,
    testBoardJump,
    testBoardMoves,
    testHasWon
    ])
  return ()

