--runghc input.hs will take test1.txt and create output file

import System.IO
import Data.Char
import Data.List
--import Data.Text (split)

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
       {-putStr "What file do you want to input: "
       file <- getLine
       inh <- openFile file ReadMode
       inpStr <- hGetContents inh   --handles all reading
       --putStrLn $ show inpStr
       let s = splitByLn inpStr
       putStrLn $ show s
       putStrLn ""
       let m = map splitBySC s
       putStrLn $ show m
       hClose inh
       --make tree with m
       --pass tree into function that recieves commends
       -}
       commands

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

--commands :: IO ()
commands = do
           putStr "comand: "
           l <- getLine
           if l == "end"
           then putStrLn "goodbye"
           else do
                putStrLn l
                commands

{-check comm 
    | comm == "end" = putStrLn goodbye
    | comm == "display" = display
    | otherwise wrong commm
    
--display = 
  
wrong comm  
    putStrLn comm ++ " is not a correct command" 
                commands
-}