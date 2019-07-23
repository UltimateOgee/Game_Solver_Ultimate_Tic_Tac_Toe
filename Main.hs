
module Main where
import Data.Char
import System.Environment
import System.Console.GetOpt
import Ultimate
import Visuals

data Flag = Help | Winrar | Depth String | Moove String deriving (Eq, Show)


options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print out a help message and quit the program"
          , Option ['w'] ["winner"] (NoArg Winrar) "Print out the best move, using an exhaustive search. Takes very long."
          , Option ['d'] ["depth"] (ReqArg Depth "<num>") "Use <num> as a cutoff depth, not default of 3."
          , Option ['m'] ["move"] (ReqArg Moove "<move>") "Makes <move> and prints resulting board. Format 'num1,num2' Ex: 0,8"
          ]


getDepth :: [Flag] -> Integer
getDepth [] = 3
getDepth ((Depth s):_) = read s
getDepth (_:flags) = getDepth flags

getMove :: [Flag] -> Move
getMove [] = (-1,-1)
getMove ((Moove s):_) = (read ([s!!0]), read ([s!!2]))
getMove (_:flags) = getMove flags


main :: IO ()
main = do
   args <- getArgs
   let (flags , others, errors) = getOpt Permute options args
   putStrLn "Reading game."
   let fname = if null others then "exampleBoard.txt" else head others
   gameStr <- readFile fname
   let gameS = case readGameState (lines gameStr) of
                    Just g  -> g
                    Nothing -> error "File was not acceptable"
   let depth = getDepth flags
   let mv = getMove flags
   if (Help `elem` flags) || (not $ null errors)
   then do putStrLn $ usageInfo "Usage: tictacgo [OPTIONS...] [file]" options
   else if Winrar `elem` flags
   then do putStrLn $ moveToString (bestMove gameS)
   else if mv /= (-1,-1)
   then do case makeMove gameS mv of
            Just g  -> putStrLn $ prettyPrintBoard $ (showBoard (g))
            Nothing -> putStrLn "That was not an acceptable move mister!"
   else putStrLn $ moveToString (goodMove gameS depth)


moveToString :: Move -> String
moveToString mv = "(" ++ show (fst mv) ++ ", " ++ show (snd mv) ++ ")"

