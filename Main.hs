import Checkers(Game, getGameMoves, makeMove, Move, getDefaultGame, gameOver, 
  gameWinner)

playGame :: Game -> IO()
playGame game = do
  if not (gameOver game) 
    then continueGame
    else finishGame where
  continueGame = do
    putStrLn $ show game
    putStrLn $ "Possible moves: \n " ++ (show $ getGameMoves game) ++ "\nEnter your move as \"Move (src_col, src_row) (des_col, des_row)\" (q to quit): "
    strMove <- getLine
    if strMove /= "q" then playGame $ applyMove game strMove else putStrLn "Quitting game..." where
  finishGame = do 
    putStrLn $ "The game is over. Final game state: \n" ++ show game
    putStrLn $ show (gameWinner game) ++ " has won"

applyMove :: Game -> String -> Game
applyMove game strMove = makeMove game move where
	move = read strMove :: Move

main :: IO ()
main = do
	putStrLn "Checkers starting"
	playGame getDefaultGame

