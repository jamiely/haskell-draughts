import Checkers(Game, getGameMoves, makeMove, Move, getDefaultGame)

playGame :: Game -> IO()
playGame game = do
	putStrLn $ show game
	putStrLn $ "Possible moves: \n " ++ (show $ getGameMoves game) ++ "\nEnter you move as \"Move (src_col, src_row) (des_col, des_row)\" (q to quit): "
	strMove <- getLine
	if strMove /= "q" then playGame $ applyMove game strMove else putStrLn "Quitting game..."

applyMove :: Game -> String -> Game
applyMove game strMove = makeMove game move where
	move = read strMove :: Move

main :: IO ()
main = do
	putStrLn "Checkers starting"
	playGame getDefaultGame

