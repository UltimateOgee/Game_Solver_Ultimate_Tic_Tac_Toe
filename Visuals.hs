module Visuals where
import Data.List.Split
import Text.Read
import Data.Maybe
import Ultimate
--FILES IN

testMe :: IO ()
testMe = do
    ourGameState <- readAFile "exampleBoard.txt"
    printBoard $ fromJust ourGameState

readAFile :: String -> IO (Maybe GameState)
readAFile fileName = do
    contents <- readFile fileName
    let gameState = (readGameState $ lines contents)
    return gameState

readGameState :: [String] -> (Maybe GameState)
readGameState linesOfFile =
    let gameBoardRows = take 9 linesOfFile
        turnData = splitOn " " $ (head (reverse linesOfFile))--linesOfFile !! 9
        smallBoards = [ zip [0..8] (map (parseSmallBoardTile) (splitOn " " x)) | x <- gameBoardRows ]
        smallBoardsPurge = (zip [0..8] [ [ ((fst tile), fromJust (snd tile)) | tile <- board, isJust (snd tile)] | board <- smallBoards ]) :: BigBoard
        currentPlayer = parsePlayer (turnData !! 0)
        currentSmallBoard = (fromJust (readMaybe (turnData !! 1))) :: Integer
        currentTurn = (currentPlayer, currentSmallBoard) :: Turn
        bigBoard = smallBoardsPurge :: BigBoard
    in Just (bigBoard, currentTurn)

parseSmallBoardTile :: String -> Maybe Player
parseSmallBoardTile chars = 
    let char = chars !! 0
    in
        case char of
            'E' -> Nothing
            'X' -> Just X
            'O' -> Just O

parsePlayer :: String -> Player
parsePlayer str =
    let char = str !! 0
    in
        case char of
            'O' -> O
            'X' -> X

--FILES OUT

prettyPrintBoard :: String -> String
prettyPrintBoard crudeBoard = (getTop firstSet)++(getMid firstSet)++(getEnd firstSet)++lineChar
                              ++(getTop secondSet)++(getMid secondSet)++(getEnd secondSet)++lineChar
                              ++(getTop thirdSet)++(getMid thirdSet)++(getEnd thirdSet)
    where 
          firstSet = take 3 parsedBoard 
          secondSet = take 3 (drop 3 parsedBoard)
          thirdSet = drop 6 parsedBoard
          parsedBoard = lines crudeBoard 
          lineChar = "===============\n"

getTop :: [String] -> String
getTop [] = "\n"
getTop (str:strs) = take 3 str ++ printIt ++ getTop strs
    where printIt = if(getTop strs == "\n") then "" else " | "

getMid :: [String] -> String
getMid [] = "\n"
getMid (str:strs) = (take 3 (drop 3 str)) ++ printIt ++ getMid strs
    where printIt = if(getMid strs == "\n") then "" else " | "

getEnd :: [String] -> String
getEnd [] = "\n"
getEnd (str:strs) = drop 6 str ++ printIt ++ getEnd strs
    where printIt = if(getEnd strs == "\n") then "" else " | "

printBoard b = putStrLn $ showBoard b

showBoard :: GameState -> String
showBoard gState = unlines $ map showLine mBoards
    where mBoards = [(lookup x (fst gState)) | x <- [0..8]]

showLine :: Maybe SmallBoard -> String
showLine Nothing = "EEEEEEEEE"
showLine smallBoard = (map convertToString mPlayers)
    where mPlayers = [lookup x (fromJust smallBoard) | x <- [0..8]]

convertToString :: Maybe Player -> Char
convertToString player = case player of
                         Just X -> 'X'
                         Just O -> 'O'
                         Nothing -> 'E'



{-
changeTile :: GameState -> Integer -> IO ()
changeTile game lastMv = do
    printLine ("Enter your next move, (current turn): " ++ (if snd game then "X" else "O")) 
    printBoard (fst game)
    requestedLoc <- getLine
    let xys = splitOn "," requestedLoc
    let loc = ((readMaybe (xys !! 0)), (readMaybe (xys !! 1))) :: (Maybe Integer, Maybe Integer)
    if (isNothing (fst loc) || isNothing (snd loc))
        then do printLine "Please input in the format of [1..9],[1..9] assuming it is bigTile,smallTile."
                changeTile game lastMv
        else do
            let parsedLoc = (fromMaybe 0 (fst loc), fromMaybe 0 (snd loc)) :: (Integer, Integer)
            let updatedGame = updateGame game ((parsedLoc, X))
            let winner = checkBigWin (fst updatedGame)
            if (parsedLoc `elem` possLocs lastMv (fst game)) 
                then 
                    if (winner /= Empty)
                        then do printBoard (fst updatedGame)
                                printLine $ (show winner) ++ " has won the game!"
                else changeTile updatedGame (snd parsedLoc)
                else do
                    printLine $ "You must play in the " ++ (show lastMv) ++ " tile on an empty property"
                    changeTile game lastMv

printBoard :: Board -> IO ()
printBoard board = do
    let boardTiles = [ getBigT board x | x <- [0..8] ]
    let topLeft = boardTiles !! 0
    let topMid = boardTiles !! 1
    let topRight = boardTiles !! 2
    let midLeft = boardTiles !! 3
    let center = boardTiles !! 4
    let midRight = boardTiles !! 5
    let botLeft = boardTiles !! 6
    let botMid = boardTiles !! 7
    let botRight = boardTiles !! 8
    
    printLine "\n\n"
    printLine "Ultimate Tic-Tac-Bros"
    printLine ""
    printLine ""

    -- 1st Row of Big Tiles

    printBoardRow topLeft 1
    printVerticalSeparator
    printBoardRow topMid 1
    printVerticalSeparator
    printBoardRow topRight 1

    printLine ""
    printHorizontalSeparator True

    printBoardRow topLeft 2
    printVerticalSeparator
    printBoardRow topMid 2
    printVerticalSeparator
    printBoardRow topRight 2

    printLine ""
    printHorizontalSeparator True

    printBoardRow topLeft 3
    printVerticalSeparator
    printBoardRow topMid 3
    printVerticalSeparator
    printBoardRow topRight 3

    printLine ""
    printHorizontalSeparator False

    -- 2nd Row of Big Tiles
    
    
    printBoardRow midLeft 1
    printVerticalSeparator
    printBoardRow center 1
    printVerticalSeparator
    printBoardRow midRight 1

    printLine ""
    printHorizontalSeparator True

    printBoardRow midLeft 2
    printVerticalSeparator
    printBoardRow center 2
    printVerticalSeparator
    printBoardRow midRight 2

    printLine ""
    printHorizontalSeparator True

    printBoardRow midLeft 3
    printVerticalSeparator
    printBoardRow center 3
    printVerticalSeparator
    printBoardRow midRight 3

    printLine ""
    printHorizontalSeparator False
    
    -- 3rd Row of Big Tiles

    printBoardRow botLeft 1
    printVerticalSeparator
    printBoardRow botMid 1
    printVerticalSeparator
    printBoardRow botRight 1

    printLine ""
    printHorizontalSeparator True

    printBoardRow botLeft 2
    printVerticalSeparator
    printBoardRow botMid 2
    printVerticalSeparator
    printBoardRow botRight 2

    printLine ""
    printHorizontalSeparator True

    printBoardRow botLeft 3
    printVerticalSeparator
    printBoardRow botMid 3
    printVerticalSeparator
    printBoardRow botRight 3

    printLine ""

    ------------- DRAWING BOARD COMPLETE --------------

printBigTileRow :: [[Tile]] -> IO ()
printBigTileRow rows = undefined

printVerticalSeparator :: IO ()
printVerticalSeparator = do
    printNoNL " || "

printHorizontalSeparator :: Bool -> IO ()
printHorizontalSeparator small = do 
    if small
    then printLine "---------- || ----------- || -----------"
    else printLine "========================================"

printBoardRow :: [Tile] -> Integer -> IO ()
printBoardRow boardTile row = do
    case row of 
     1 -> printNoNL $ (propertyToString (snd (boardTile !! 0))) ++ " | " ++ (propertyToString (snd (boardTile !! 1))) ++ " | " ++ (propertyToString (snd (boardTile !! 2)))
     2 -> printNoNL $ (propertyToString (snd (boardTile !! 3))) ++ " | " ++ (propertyToString (snd (boardTile !! 4))) ++ " | " ++ (propertyToString (snd (boardTile !! 5)))
     3 -> printNoNL $ (propertyToString (snd (boardTile !! 6))) ++ " | " ++ (propertyToString (snd (boardTile !! 7))) ++ " | " ++ (propertyToString (snd (boardTile !! 8)))


--boardRowToString :: SmallBoard -> Integer -> [Char]
--boardRowToString boardTile row =
--    case row of
--    1 -> (propertyToString (snd (boardTile !! 0))) ++ (propertyToString (snd (boardTile !! 1))) ++ (propertyToString (snd (boardTile !! 2)))
--    2 -> (propertyToString (snd (boardTile !! 3))) ++ (propertyToString (snd (boardTile !! 4))) ++ (propertyToString (snd (boardTile !! 5)))
--    3 -> (propertyToString (snd (boardTile !! 6))) ++ (propertyToString (snd (boardTile !! 7))) ++ (propertyToString (snd (boardTile !! 8)))

-- Write Read I/O stuff

--lookupSmallB :: SmallBoard -> Int -> Char
--lookupSmallB smallB pos =
--    let results = [ x | x <- smallB, (fst x) == pos ]
--    in if (length results) == 0
--    then 

printLine :: String -> IO ()
printLine str = do
    putStr $ str ++ "\n"

printNoNL :: String -> IO ()
printNoNL str = do
    putStr $ str ++ " " 

propertyToString :: Property -> String
propertyToString X = "X"
propertyToString O = "O"
propertyToString Empty = " "

-}
