strToIntList :: String -> [Int]
strToIntList s = fmap read $ words s

strToInt :: String -> Int
strToInt s = read s

dist :: Int -> Int -> Int
dist player pos
	| player == 1 = pos 
	| otherwise = 10-pos


money :: Int -> [Int] -> [Int] -> Int
money player first second
	| player == 1 = 100 - sum first
	| otherwise = 100 - sum second

other :: Int -> Int
other player
	| player == 1 = 2
	| otherwise = 1

runGame :: Int -> Int -> [Int] -> [Int] -> Int
runGame player pos first second =
	div (money player first second) (dist player pos)

main :: IO()
main = do
	-- Take input
	player <- getLine
	pos <- getLine
	firstMoves <- getLine
	secondMoves <- getLine
	putStrLn $ show $ runGame (strToInt player) (strToInt pos) (strToIntList firstMoves) (strToIntList secondMoves)

