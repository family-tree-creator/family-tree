--input that prints directly

import System.IO
import Control.Monad
import Data.Char
import Data.List

main = do
	putStrLn "???"
	name <- getLine
	putStrLn(name)

--function to split inputted string by delimiter 
splitBySemiColon :: String -> [String]
splitBySemiColon "" = [""]
splitBySemiColon (x:xs)
	| x == delimiter = "": mid
	| otherwise = (x : head mid) : tail mid
	where
		mid = splitBySemiColon xs

--delimiter set to ';'
delimiter = ';'
