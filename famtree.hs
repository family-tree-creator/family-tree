--
-- Famtree.hs
--

--tree in three forms: empty, single individual, married w/ children
data FamTree a = Empty | Node a [FamTree a]
  deriving (Show, Read, Eq)

--tree for single individual
singleTree :: a -> FamTree a
singleTree a = Node a []

--data FamTree a = Empty | Node {
--		root :: a,
--		children :: FamTree a 
--		} deriving (Show, Read, Eq) 

--add child to parent
addChild :: (Eq a) => a -> FamTree a -> FamTree a
addChild c Empty = singleTree c
addChild c (Node p list) 
	| c == p = singleTree p
	| list == [] = Node p (Node c[]:[]) 
	| c `famElem` list = Node p list
	| otherwise = Node p (list ++ [Node c[]])

--add child version 2
addChild' :: (Eq a) => FamTree a -> FamTree a -> FamTree a
addChild' (Node c cList) Empty = (Node c cList)
addChild' (Node c cList) (Node p list) 
	| c == p = (Node p list)
	| list == [] = Node p (Node c cList:[])
	| c `famElem` list = Node p list   				--erases cList
	| otherwise = Node p (list ++ [Node c cList])

--checks if element is part of a family tree
famElem :: (Eq a) => a -> [FamTree a] -> Bool
famElem a [] = False
famElem a (Node x(xChild):xs) 
	| a == x = True
	| xChild /= [] = a `famElem` xChild
	| otherwise = a `famElem` xs 


--should Individual be type instead? 
--Individual :: [(String,String,[Int],[Int],Int)]
data Individual = Individual { firstName :: String
						     , lastName :: String
						     , birth :: [Int]
						     , death :: [Int]
						     , age :: Int
						   } deriving (Show, Read, Eq)
						   
printIndividual :: Individual -> String
printIndividual (Individual {firstName = f,lastName = l, birth = b, death = d, age = a}) = f ++
     " , " ++ l ++ ". Birth date: " ++ show b ++ " Death date: " ++ show d ++" Age: " ++ show a
 

 
--prints curr or root of tree
--toList :: famTree a -> Node 
--toList (Empty) = Empty
--toList (Node) = Node
--toList (Node (xs)) = Node 

--sample = Node "Will" [Node "Willow" [], Node "Jaden" []]

