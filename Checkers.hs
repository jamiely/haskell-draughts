module Checkers ( Game ) where

import Test.HUnit

type Player = String

newtype Size = (Int, Int) -- Width, Height
newtype Position = (Int, Int) -- row, col

data Game = Game GameState [Player] deriving (Show)
data Board = Board Size [[Marker]] deriving (Show) -- Width, Height, Markers
data GameState = GameState Board Player deriving (Show) -- State, Board, Current Player
data Marker = None | Black | Red deriving (Show, Eq)

-- | Returns a list of Markers representing a board
getBoard :: Size -> Board
getBoard (width, height) = board where
  board = Board width height markers
  markers = take height $ repeat markerRow
  markerRow = take width $ repeat None

size :: Board -> Size
size (Board s _) = s

-- | Retrieve the markers of a board
markers :: Board -> [Marker]
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

  upR r c (m:ns) mrk
    | r == 0    = upC c m mrk
    | otherwise = m : upR (r-1) c ns mrk
  upR _ _ [] _ = [] 

  upC c (mi:mis) mrk
    | c == 0    = mrk : mis
    | otherwise = mi : upC (c-1) mis mrk
  upC _ [] _ = []

testGetBoard :: Test
testGetBoard = "getBoard" ~: TestList [
  getBoard 2 3 ~?= [None, None, None, None, None, None]
  ]

test :: IO ()
test = do
  runTestTT (TestList [
    testGetBoard
    ])
  return ()

