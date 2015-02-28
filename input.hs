--runghc input.hs will take test1.txt and create output file

import System.IO
import Data.Char
import Data.List

main :: IO ()
main = do 
       inh <- openFile "test1.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh   --handles all reading
       let split = unwords (splitBySemiColon inpStr)  --separates input
       hPutStr outh split
       hClose inh
       hClose outh


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
