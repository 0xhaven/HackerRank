module Main where
slice start end = take (end - start + 1) . drop start

match x = all (==x)

isFull = not.(any (=="_"))

isWon player board = (match player $ slice 0 2 board) || (match player $ slice 3 5 board) || (match player $ slice 6 8 board)
    || (match player [board!!0,board!!3,board!!6]) || (match player [board!!1,board!!4,board!!7]) || (match player [board!!2,board!!5,board!!8])
    || (match player [board!!0,board!!4,board!!8]) || (match player [board!!2,board!!4,board!!6])

other player =
    if player=="X"
        then "O"
        else "X"
        
baseValue player board =
    if isWon player board
        then 1
        else if isWon (other player) board
            then -1
            else 0

bestpair (id1,(val1,depth1)) (id2,(val2,depth2)) = 
    if val2 > val1 || (val2 == val1 && ((val2==1 && depth2 < depth1) || (val2/=1 && depth1<depth2)))
    then (id2,(val2,depth2))
    else (id1,(val1,depth1))

data Pos = Pos Int Int
    deriving(Eq,Show)

nullpos = Pos (-1) (-1)
nextpos :: Pos -> Pos
nextpos (Pos x y) =
    if (Pos x y) == nullpos
    then nullpos
    else if y<2
        then Pos x (y+1) 
        else if x<2
            then Pos (x+1) 0
            else nullpos

nextMove' :: String -> [String] -> [String] -> (Pos,(Int,Int)) -> Pos -> (Pos,(Int,Int))

nextMove' player (b:boardend) boardfront bestmove pos =
    if b=="_"
        then nextMove' player boardend (boardfront++[b]) newbest (nextpos pos)
        else nextMove' player boardend (boardfront++[b]) bestmove (nextpos pos)
    where newbest = bestpair bestmove $ (pos, value player (boardfront++player:boardend))
    
nextMove' player [] boardfront bestmove pos = bestmove


newmove player board = nextMove' player board [] (nullpos, (-infinity,0)) startpos
    where
        infinity = 100
        startpos = Pos 0 0


--Board value to player immediatly /after/ they have moved.
value :: String -> [String] -> (Int, Int)

value player board = 
    if isFull board || (baseValue player board)/=0
        then (baseValue player board, 0)
        else (-fst deeper, 1 + snd deeper)
    where deeper = snd $ newmove (other player) board
            
-- player == "X" or "O" representing the next move to be made.
-- board is a list of strings with 'X', 'O' or '_' :- the current state of the board
-- return a pair (x,y) which is your next move

nextMove player board = fst $ newmove player board




getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

flatten :: [String] -> [String]
flatten strs = map (\c->[c]) $ flatten' [] strs
flatten' result [] = result
flatten' result (x:xs) = flatten' (result++x) xs

main = do

    -- If player is X, I'm the first player.
    -- If player is O, I'm the second player.
    player <- getLine


    -- Read the board now. The board is a list of strings filled with X, O or _.
    rawboard <- getList 3
    -- Split each string and flatten the array
    board <- return $ flatten rawboard
    -- Proceed with processing and print 2 integers separated by a single space.

    --putStrLn.show $ board   
    
    --putStrLn $ "baseValue: "++(show $ baseValue player board)
    --putStrLn $ "value to "++(other player)++": "++(show $ value (other player) board)
    --putStrLn $ "Move: "++(show $ newmove player board)
    
    
    putStrLn.(\(Pos x y) -> show x ++ " " ++ show y).nextMove player $ board



