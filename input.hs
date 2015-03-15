--runghc input.hs will take test1.txt and create output file

import System.IO
import Data.Char
import Data.List
import Data.Time
import FamTree
import Data.Maybe

--input functions
--look: looks at nodes info
--father: make father curr
--mother: makes mother curr
--child (Int): makes child # curr
--child (String,String): makes child (S,S) curr
--nextChildren: displays next set of children if they exist
--prevChildren: displays prev set of children if they exist
--help: displays list of functions with discription
--end: stops program
--display: displays tree again

main :: IO ()
main = do 
       putStr "What file do you want to input: "
       file <- getLine
       inh <- openFile file ReadMode
       inpStr <- hGetContents inh   --handles all reading
       --putStrLn $ show inpStr
       --let s = splitByLn inpStr
       --putStrLn $ show s
       --putStrLn ""
       --let people = map splitBySC s
       --putStrLn $ show people
       --hClose inh
       --make tree with m
       --pass tree into function that recieves commends
       time <- getCurrentTime
       let (y,m,d) = toGregorian $ utctDay time
       let date = (m,d, fromIntegral y) 
       let tree = goToRoot (makeTree (map splitBySC (splitByLn inpStr)) (m,d, fromIntegral y))
       putStrLn ("\n" ++ (printFamTree tree) ++ "\n")
       commands tree
       hClose inh

splitByLn :: String -> [String]
splitByLn = splitBy '\n'

splitBySC :: String -> [String]
splitBySC = splitBy ';'

splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy c (x:xs)
    | x == c = "": mid
    | otherwise = (x : head mid) : tail mid
    where
        mid = splitBy c xs
        
checkCommand :: [String] -> Bool
checkCommand ("moveTo":ls) = (length ls) == 2
checkCommand ("to":ls)     = (length ls) == 2
checkCommand ("end":ls)    = True
checkCommand _             = False

executeCommand :: [String] -> Maybe FTZipper -> Maybe FTZipper
executeCommand  ("moveTo":ls) tree = let result = ftSearch (head ls, head(tail ls)) 'a' (goToRoot tree)
                                     in if Data.Maybe.isJust result
                                        then result
                                        else error ((show ls) ++ ": is not a person")
executeCommand ("to":ls) tree = let result = ftTo (head ls, head(tail ls)) tree
                                in if Data.Maybe.isJust result
                                   then result
                                   else error ((show(head ls, head(tail ls))) ++ ": is not a person")

commands :: Maybe FTZipper -> IO ()
commands tree = do putStr "command: "
                   l <- getLine
                   --putStrLn l
                   let (comm:ls) = splitBy ' ' l
                   --putStrLn $ show (checkCommand (comm:ls))
                   --putStrLn $ show (goToRoot tree)
                   if checkCommand (comm:ls)
                   then if comm == "end"
                        then putStrLn "goodbye"
                        else let newTree = executeCommand (comm:ls) tree
                             in do putStrLn ("\n" ++ (printFamTree newTree) ++ "\n")
                                   commands newTree
                   else do putStrLn (l ++ ": is an incorrect command\n")
                           commands tree

{-check comm 
    | comm == "end" = putStrLn goodbye
    | comm == "display" = display
    | otherwise wrong commm
    
--display = 
  
wrong comm  
    putStrLn comm ++ " is not a correct command" 
                commands
-}