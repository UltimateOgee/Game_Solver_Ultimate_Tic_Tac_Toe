--import Visuals
module Ultimate where
import Data.Maybe
import Debug.Trace
import Data.List
import Text.Read
import Debug.Trace


--A Location refers to a point on the Board, where the first Int refers to the "larger" square on the Tic Tac Toe board,
--and the 2nd Int refers to the "smaller" square withing the "larger" square of the Board. 
--type Location = (Integer, Integer)

--type Move = Location
--type Move = Tile

--A Property refers to who "owns" a Tile. Either X, O, or Empty (No one owns).
data Player = X | O deriving (Eq, Show)

--A Turn refers to a player whose turn it is and the int location of the big tile they must play in (-1 if their choice).
type Turn = (Player, Integer)
type Move = (Integer, Integer) 


--remove ongoing
data Winner = Won Player | Ongoing | Tie deriving (Eq, Show)

--A Tile refers to a location on the board and who owns said location
--type Tile = (Location, Property)

type SmallBoard = [(Integer, Player)]
type BigBoard = [(Integer, SmallBoard)]

--A board is a list of Tiles.
--type Board = [Tile]

type GameState = (BigBoard, Turn)

wins = [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [6,4,2]]
initialBoard = map (\x -> (x, [])) [0..8]
xStart = (initialBoard, (X, -1))
oStart = (initialBoard, (O, -1))
allMoves = [(x,y) | x <- [0..8], y <- [0..8]]

exampleBoard  = [(0, [(0,X), (4,O), (5,X)]), (1, [(3,X), (4,O)]), (2, []), (3, []), (4, []), (5, []), (6, []), (7, []), (8, [])]


subList :: [Integer] -> [Integer] -> Bool
subList a b = all (`elem` b) a

--Same as last checkSmallWin, except it returns a Winner instead of a property.
checkSmallWin :: SmallBoard -> Winner
checkSmallWin [] = Ongoing
checkSmallWin brd | any (\x -> subList x xNums) wins = Won X
                  | any (\x -> subList x oNums) wins = Won O
                  | subList [0..8] allNums           = Tie
                  | otherwise                        = Ongoing 
           where allNums = map fst brd 
                 xNums   = [num | (num, player) <- brd, player == X]  
                 oNums   = [num | (num, player) <- brd, player == O]

--Same as last checkBigWin, except it returns a Winner instead of a property.
checkBigWin :: BigBoard -> Winner --return Maybe Winner
checkBigWin brd | any (\x -> subList x xNums) wins = Won X
                | any (\x -> subList x oNums) wins = Won O
                | subList [0..8] tNums             = Tie
                | otherwise                        = Ongoing
            where bigWins = [(num, checkSmallWin(sBoard)) | (num, sBoard) <- brd]
                  xNums = [num | (num, winner) <- bigWins, winner == Won X]
                  oNums = [num | (num, winner) <- bigWins, winner == Won O]
                  tNums = [num | (num, winner) <- bigWins, winner == Tie]

--Recursive function that returns all of the moves played given a BigBoard.
usedMoves :: BigBoard -> [Move]
usedMoves [] = []  
usedMoves ((num, sBrd) : bs) = (map (\x -> (num, x)) smallNums) ++ usedMoves bs
                        where smallNums = map fst sBrd

validMoves :: GameState -> [Move]
validMoves (brd, (player, req)) = if req == -1
                                  then openMoves
                                  else filter (\(big, _) -> big == req) openMoves
                 where openMoves = filter (\x -> not (x `elem` (usedMoves brd))) allMoves

changePlayer :: GameState -> Player
changePlayer (_, (player,_)) | player == X = O
                             | otherwise = X

findSBoard :: BigBoard -> Integer -> SmallBoard
findSBoard brd num = case lookup num brd of
                        Just sBoard -> sBoard
                        Nothing     -> error "no small board found"


updateSBoard :: Integer -> Player -> SmallBoard -> SmallBoard
updateSBoard mv plyr brd = case lookup mv brd of 
                             Nothing -> (mv, plyr) : brd
                             _       -> error "how could this happen"

updateBBoard :: Move -> Player -> BigBoard -> BigBoard
updateBBoard mv plyr (b:bs) = if fst mv /= fst b
                              then b : updateBBoard mv plyr bs
                              else (fst b, updateSBoard (snd mv) plyr (snd b)) : bs 

makeMove :: GameState -> Move -> Maybe GameState
makeMove gState@(board,(pl,loc)) (big, small) | (big, small) `elem` vMoves = Just (updatedBoard, (nextP, reqMv)) 
                                              | otherwise = Nothing
       where updatedBoard = updateBBoard (big, small) (fst (snd gState)) (fst gState)
             nextP        = changePlayer gState
             vMoves       = validMoves gState
             sBoard       = findSBoard (fst gState) small
             reqMv        = if checkSmallWin sBoard == Ongoing
                            then small
                            else -1  

bestWinner :: Player -> [Winner] -> Winner
bestWinner pl winners | Won pl `elem` winners = Won pl
                      | Tie `elem` winners = Tie
                      | pl == X = Won O
                      | otherwise = Won X

whoWillWin :: GameState -> Winner
whoWillWin gState@(board, (pl, req)) | current /= Ongoing = current
                                     | otherwise = bestWinner pl $ map whoWillWin vGStates
                 where current  = checkBigWin board
                       vMoves   = validMoves gState
                       vGStates = catMaybes $ map (\m -> makeMove gState m) vMoves

xWinGame = ([(0,[(5,X), (6,O), (8,X)]), (1, [(4,O), (8,O)]), (2, [(0,O)]), (3,[]), (4, [(0,O), (1,X), (4,X), (7,X)]), (5, [(8,O)]), (6, []), (7, [(4,O)]), (8, [(0,X), (1,X), (2,X), (4,O)])], (X, 0))

bestMove :: GameState -> Move
bestMove gState@(board, (pl, req)) = case lookup (bestWinner pl (map fst moveWins)) moveWins of
                                            Just move -> move
                                            _         -> error "how could this happen to me! I made my mistakes! Got nowhere to run! The night goes onnnnnn as I'm fading away"
                where vMoves = validMoves gState
                      vGStates = zip (catMaybes (map (\m -> makeMove gState m) vMoves)) vMoves
                      moveWins = map (\(g, m) -> (whoWillWin g, m)) vGStates

evalPart1 :: [SmallBoard] -> Integer
evalPart1 [] = 0
evalPart1 (sb:sbs) = case checkSmallWin sb of 
                        Won X -> 100 + evalPart1 sbs
                        Won O -> (-100) + evalPart1 sbs
                        _     -> evalPart2 sb + evalPart1 sbs

evalPart2 :: SmallBoard -> Integer
evalPart2 sBoard = sum $ map helper outOwners
            where outOwners = [[lookup x sBoard | x <- outs] | outs <- wins]
                  helper :: [Maybe Player] -> Integer
                  helper ms | Just X `elem` ms && Just O `elem` ms = 0
                            | length (filter (\p -> p /= Nothing) ms) /= 2 = 0
                            | otherwise = if Just X `elem` ms then 5 else -5
                  
eval :: GameState -> Integer 
eval gState@(board, (pl, req)) | current == Won X = 10000
                               | current == Won O = -10000
                               | current == Tie = 0
                               | otherwise = evalPart1 (map snd board) 
                        where current = checkBigWin board

bestRating :: Player -> [Integer] -> Integer
bestRating pl ratings | pl == X = maximum ratings
                      | otherwise = minimum ratings

recEval :: GameState -> Integer -> Integer
recEval gState@(board, (pl, req)) 0 = eval gState
recEval gState@(board, (pl, req)) depth = bestRating pl (map (\g -> recEval g (depth-1)) vGStates)
                        where vGStates = catMaybes (map (\m -> makeMove gState m) (validMoves gState))

pickMove :: Player -> [(Integer, Move)] -> Move
pickMove pl moveEvals | pl == X = snd (maximum moveEvals)
                    | otherwise = snd (minimum moveEvals)

goodMove :: GameState -> Integer -> Move
goodMove gState@(board, (pl, req)) depth = pickMove pl ratingsNMoves
                    where vMoves = validMoves gState
                          statesNMoves  = zip (catMaybes (map (\m -> makeMove gState m) vMoves)) vMoves
                          ratingsNMoves = map (\(g, m) -> (recEval g depth, m)) statesNMoves --Starting recEval at (depth-1) with the value of depth, this is why a depth greater than 4 takes pretty long(I think)
