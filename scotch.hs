strToIntList :: String -> [Int]
strToIntList s = fmap read $ words s

strToInt :: String -> Int
strToInt s = read s

other player
    | player == 1 = 2
    | player == 2 = 1
    | otherwise = player

runGame player pos first s
    --logic here
    | player == 1 = "10"
    | player == 2 = "10"
    | otherwise = "Rest"

main :: IO()
main = do
    -- Take input
    player <- getLine
    pos <- getLine
    firstMoves <- getLine
    secondMoves <- getLine
    putStrLn $ runGame (strToInt player) (strToInt pos) (strToIntList firstMoves) (strToIntList secondMoves)

