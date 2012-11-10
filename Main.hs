import Checkers(Game, getGameMoves, makeMove, Move, getDefaultGame, gameOver, 
  gameWinner)

playGame :: Game -> IO()
playGame game = do
  if not (gameOver game) 
    then continueGame
    else finishGame where
  possibleMoves = getGameMoves game
  isValidMove move = move `elem` possibleMoves
  continueGame = do
    putStrLn $ show game
    putStrLn $ "Possible moves: \n " ++ (show $ possibleMoves) ++ "\nEnter your move as \"Move (src_col, src_row) (des_col, des_row)\" (q to quit): "
    strMove <- getLine
    if strMove == "q"
      then putStrLn "Quitting game..." 
      else if isValidMove (getMove strMove)
        then playGame $ applyMove game strMove 
        else putStrLn $ strMove ++ " is not a valid move"
  finishGame = do 
    putStrLn $ "The game is over. Final game state: \n" ++ show game
    putStrLn $ show (gameWinner game) ++ " has won"

getMove :: String -> Move
getMove strMove = read strMove :: Move

applyMove :: Game -> String -> Game
applyMove game strMove = makeMove game $ getMove strMove

main :: IO ()
main = do
	putStrLn "Checkers starting"
	playGame getDefaultGame

