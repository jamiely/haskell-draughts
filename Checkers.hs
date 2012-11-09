module Checkers ( Game, testAll, getGameMoves, makeMove, Move, 
  getDefaultGame, gameOver, gameWinner ) where

import Test.HUnit
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Map (Map)

type Size = (Int, Int) -- Width, Height
type Position = (Int, Int) -- row, col
type PositionMap = Map Position Marker

data Player = Player Marker deriving (Show, Eq)
data Game = Game GameState [Player] deriving (Show, Eq)
data Board = Board Size PositionMap deriving (Eq) -- Width, Height, Mark
data GameState = GameState Board Player deriving (Show, Eq) -- State, Board, Current Player
data Marker = None | Black | Red | King Marker deriving (Show, Eq)
data Move = Move Position Position deriving (Show, Eq, Read)

instance Show Board where
  show board@(Board (width, height) posMap) = join rows where
    rows = addColNums 8 $ addRowNums 1 $ chunk width markers
    addRowNums start (r:rs) = (show start ++ " " ++ r) : addRowNums (start + 1) rs
    addRowNums _ [] = []
    addColNums count rs = rs ++ ["  " ++ (foldr (\d s-> s ++ show d) "" (reverse [1..count]))]
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
isInBounds board pos = posX > 0 && posX <= boardX && posY > 0 && posY <= boardY where
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
  otherMarker = basicMarker marker
  eqMarker m a = basicMarker a == m
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

gameBoard :: Game -> Board
gameBoard (Game (GameState board _) _) = board

gameOver :: Game -> Bool
gameOver game = any (hasWon board) markers where
  board = gameBoard game
  markers = [Red, Black]

gameWinner :: Game -> Marker
gameWinner game@(Game (GameState b (Player m)) _) = winner where
  winner = if gameOver game then
                            if m == Red then Black
                                        else Red
                            else None

-- | Clears the position
clearStatePos :: GameState -> Position -> GameState
clearStatePos orig posSrc = replaceStatePos orig posSrc None

-- | Replaces the item in the position with an arbitrary marker
replaceStatePos :: GameState -> Position -> Marker -> GameState
replaceStatePos orig@(GameState origBoard origPlayer) posSrc newMarker = 
  GameState newBoard origPlayer where
  newBoard = updateBoard origBoard posSrc newMarker

getStateMarkerAt :: GameState -> Position -> Marker
getStateMarkerAt (GameState board _) pos = 
  markerAt (markers board) pos

-- | Updates the game state by making a move at the 
-- passed position
updateState :: GameState -> Position -> Position -> GameState
updateState orig@(GameState origBoard origPlayer) posSrc posDes = 
  GameState newBoard next where
  sourceMarker = markerAt (markers origBoard) posSrc
  removeBoard = updateBoard origBoard posSrc None
  newBoard = updateBoard removeBoard posDes sourceMarker
  next = case origPlayer of 
    Player Black        -> Player Red
    Player (King Black) -> Player Red
    _                   -> Player Black

testUpdateState :: Test
testUpdateState = "updateState" ~: TestList [
  updateState origState posSrc posDes ~?= newState
  ] where
  origState = GameState origBoard $ Player Black
  newState = GameState newBoard $ Player Red
  posDes = (1, 2)
  posSrc = (1,1) -- can be anything
  sz = (3, 2)
  origBoard = getBoard sz
  newBoard = updateBoard origBoard posDes Black


-- | Given a Board and a position on a board, returns the valid
-- moves for the marker. Markers may move "forward" diagonally,
-- unless they are King-ed, in which case they may move in any
-- direction. Markers may jump markers of the opposing player to move
-- two spaces diagonally. 
boardMoves :: Board -> Position -> [Position]
boardMoves board src = boardWalk board src ++ boardJump board src

testBoardMoves :: Test
testBoardMoves = "Test boardMoves" ~: TestList [
  boardMoves board2 startPos ~?= [(2,4),(4,4)],
  boardMoves board3 startPos ~?= [(2,4),(5,5)],
  boardMoves board4 (3,5) ~?= [(2,4),(5,3)]
  ] where
  board1 = getBoard(5,5)
  board2 = updateBoard board1 (3,3) Black
  board3 = updateBoard board2 (4,4) Red
  board4 = updateBoard board3 (3,5) (King Black)
  -- 4..K..
  -- 3...R.
  -- 2..B..
  -- 1.....
  -- 0.....
  --  01234
  startPos = (3,3)

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
testBoardJump = "Test boardJump" ~: TestList [
  boardJump board3 startPos ~?=  [ (5,5)],
  boardJump board4 startPos ~?=  [ (5,5)],
  boardJump board5a startPos ~?= [ (5,5), (1,5)],
  boardJump board5b startPos ~?= [ (5,5)],
  boardJump board6 startPos ~?=  [ (5,5)]
  ] where
  board1 = getBoard(5, 5) 
  board2 = updateBoard board1 (3, 3) Black
  board3 = updateBoard board2 (4, 4) Red
  board4 = updateBoard board3 (2, 2) Red
  board5a = updateBoard board4 (2, 4) Red
  board5b = updateBoard board4 (2, 4) Black
  board6 = updateBoard board5a (1, 5) Red
  startPos = (3, 3) -- center of board

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

-- | Retrieves the possible moves from the passed position
getStateMoves :: GameState -> Position -> [Move]
getStateMoves state@(GameState board@(Board sz posMap) player) posSrc = moves where
  moves = if (markerMatches player marker) 
            then map (Move posSrc) (boardMoves board posSrc) else []
  marker = markerAt posMap posSrc
  markerMatches (Player m1) m2 = basicMarker m1 == basicMarker m2

testGetStateMoves :: Test
testGetStateMoves = "Test getStateMoves" ~: TestList [
  getStateMoves state pos1 ~?= [Move (1,3) (2,4)]
  ] where
  state = GameState board $ Player Black
  emptyBoard = getDefaultBoard
  board = emptyBoard
  pos1 = (1,3)

getPlayerPositions :: Player -> Board -> [Position]
getPlayerPositions player board = positions where
  marker = case player of 
             (Player m) -> basicMarker m
  matchesMarker pos = marker == basicMarker (markerAt (markers board) pos)
  positions = filter matchesMarker $ boardPositions board

getAllStateMoves :: GameState -> [Move]
getAllStateMoves state@(GameState board player) = moves where
  moves = concat $ map (getStateMoves state) positions
  positions = boardPositions board

getGameMoves :: Game -> [Move]
getGameMoves (Game gs _) = getAllStateMoves gs

-- | Un-kings the marker
basicMarker :: Marker -> Marker
basicMarker (King m) = basicMarker m
basicMarker Black = Black
basicMarker Red = Red
basicMarker None = None

makeMove :: Game -> Move -> Game
makeMove game@(Game gs players) move@(Move p1 p2) = newGame where
  newGame = Game newState3 players
  newState1 = (updateState gs p1 p2)
  newState2 = if isJump move 
                then clearStatePos newState1 jumpedPos
                else newState1
  newState3 = if (shouldKingPosition movedMarker p2)
                then (kingPieceAt newState2 p2)
                else newState2

  movedMarker = getStateMarkerAt newState2 p2

  jumpedPos = getJumpedPosition move

kingPieceAt :: GameState -> Position -> GameState
kingPieceAt gs pos = newState where
  newState = replaceStatePos gs pos kingedPiece
  kingedPiece = kingPiece $ getStateMarkerAt gs pos

isJump :: Move -> Bool
isJump (Move (sc,sr) (dc,dr)) = diff > 1 where 
  diff = abs $ sr - dr

getJumpedPosition :: Move -> Position
getJumpedPosition m@(Move (sc,sr) (dc,dr)) = mPos where
  mPos = (sc+deltaColumn,sr+deltaRow)
  deltaColumn = if dc > sc then 1 else -1
  deltaRow = if dr > sr then 1 else -1

-- | determines whether a piece in a position should be kinged
shouldKingPosition :: Marker -> Position -> Bool
shouldKingPosition (King _) _ = False
shouldKingPosition Black (_, 8) = True
shouldKingPosition Red (_, 1) = True
shouldKingPosition _ _ = False

-- | Kings a black or red piece and leaves other pieces untouched
kingPiece :: Marker -> Marker
kingPiece Black = King Black
kingPiece Red = King Red
kingPiece m = m

getTestGame1 :: Game
getTestGame1 = game1 where
  game1 = foldl makeMove getDefaultGame moves
  moves = [Move (1,3) (2,4), Move (2,6) (1,5), Move (3,3) (4,4), Move (1,5) (3,3), Move (4,4) (3,5), Move (8,6) (7,5), Move (3,5) (4,4), Move (7,5) (6,4), Move (2,2) (1,3), Move (7,7) (8,6), Move (1,3) (2,4), Move (6,8) (7,7), Move (1,1) (2,2), Move (3,3) (1,1), Move (3,1) (2,2), Move (1,1) (3,3), Move (2,4) (3,5), Move (3,3) (5,5), Move (4,2) (3,3), Move (6,4) (4,2), Move (6,2) (5,3)]

getTestGame2 = makeMove getTestGame1 $ Move (4,2) (3,1)

testGame1 :: Test
testGame1 = "Test testGame1" ~: TestList [
  getDefaultGame ~?= getTestGame1
  ] 

testAll :: IO ()
testAll = do
  runTestTT (TestList [
    testGetBoardPositionMap,
    testGetBoard,
    testSwap,
    testUpdateBoard,
    {-testUpdateState,-}
    testBoardJump,
    testBoardMoves,
    testGetStateMoves,
    testHasWon,
    testGame1
    ])
  return ()

