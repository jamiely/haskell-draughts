module Checkers ( Game ) where

import Test.HUnit

type Player = Marker

type Size = (Int, Int) -- Width, Height
type Position = (Int, Int) -- row, col

data Game = Game GameState [Player] deriving (Show)
data Board = Board Size [[Marker]] deriving (Show, Eq) -- Width, Height, Markers
data GameState = GameState Board Player deriving (Show, Eq) -- State, Board, Current Player
data Marker = None | Black | Red deriving (Show, Eq)

-- | Returns a list of Markers representing a board
getBoard :: Size -> Board
getBoard sz@(width, height) = board where
  board = Board sz markers
  markers = take height $ repeat markerRow
  markerRow = take width $ repeat None

testGetBoard :: Test
testGetBoard = "getBoard" ~: TestList [
  markers (getBoard (3, 2)) ~?= [[None, None, None], [None, None, None]]
  ]

size :: Board -> Size
size (Board s _) = s

-- | Retrieve the markers of a board
markers :: Board -> [[Marker]]
markers (Board _ ms) = ms

-- | Update the markers associated with a board
swapMarkers :: Board -> [[Marker]] -> Board
swapMarkers (Board sz _) ms = Board sz ms

-- | Use to update the mark on the board
updateBoard :: Board -> Position -> Marker -> Board
updateBoard board (row, col) mark = newBoard where
  ms = markers board
  newBoard = swapMarkers board newMarks
  newMarks = upR row col ms mark

  upR :: Int -> Int -> [[Marker]] -> Marker -> [[Marker]]
  upR r c (m:ns) mrk
    | r == 0    = (upC c m mrk):ns
    | otherwise = m : upR (r-1) c ns mrk
  upR _ _ [] _ = [] 

  upC :: Int -> [Marker] -> Marker -> [Marker]
  upC c (mi:mis) mrk
    | c == 0    = mrk : mis
    | otherwise = mi : upC (c-1) mis mrk
  upC _ [] _ = []

testUpdateBoard :: Test
testUpdateBoard = "updateBoard" ~: TestList [
  markers origBoard ~?= [[None, None, None], [None, None, None]],
  updateBoard origBoard (0,1) Black ~?= 
    Board sz [[None, Black, None], [None, None, None]]
  ] where
  sz = (3,2)
  origBoard = getBoard sz

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

