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

-- add child to parent: (delete?)
-- child added can just be String
-- >addChild "child" (Node "parent" []) 
-- | Node "parent" [Node "child []]
addChild :: (Eq a) => a -> FamTree a -> FamTree a
addChild c Empty = singleTree c
addChild c (Node p list) 
	| c == p = singleTree p
	| list == [] = Node p (Node c[]:[]) 
	| c `famElem` list = Node p list
	| otherwise = Node p (list ++ [Node c[]])

--add child version 2: child added is already a Node
-- >addChild' (Node "child" []) (Node "parent" [])
addChild' :: (Eq a) => FamTree a -> FamTree a -> FamTree a
addChild' (Node c cList) Empty = (Node c cList)
addChild' (Node c cList) (Node p list) 
	| c == p = (Node p list)
	| list == [] = Node p (Node c cList:[])
	| c `famElem` list = Node p list   				--erases cList
	| otherwise = Node p (list ++ [Node c cList])

--add parent to child. 
-- parent added just String (delete?)
addParent :: (Eq a) => a -> FamTree a -> FamTree a
addParent p Empty = singleTree p
addParent p (Node c cList) 
	| p == c = (Node c cList)
	| otherwise = Node p (Node c cList:[])

--add parent version 2: Node parent added to Node child
addParent' :: (Eq a) => FamTree a -> FamTree a -> FamTree a
addParent' (Node p list) Empty = (Node p list)
addParent' (Node p list) (Node c cList)
	| p == c = (Node c cList)
	| otherwise = addChild' (Node c cList) (Node p list)


--checks if element is part of a family tree
famElem :: (Eq a) => a -> [FamTree a] -> Bool
famElem a [] = False
famElem a (Node x(xChild):xs) 
	| a == x = True
	| xChild /= [] = a `famElem` xChild
	| otherwise = a `famElem` xs 

--datatype to hold information about individual
data Individual = Individual { firstName :: String
						     , lastName :: String
						     , birth :: [Int]
						     , death :: [Int]
						     , age :: Int
						   } deriving (Show, Read, Eq)
						   
printIndividual :: Individual -> String
printIndividual (Individual {firstName = f,lastName = l, birth = b, death = d, age = a}) = f ++
     " , " ++ l ++ ". Birth date: " ++ show b ++ " Death date: " ++ show d ++" Age: " ++ show a 
 
--prints curr or root of tree. Not useful yet
printRoot :: FamTree a -> FamTree a 
printRoot Empty = Empty
printRoot (Node x list) = Node x []

--test variables
a = Node "Ann" []
b = Individual "Ann" "Whal" [01,24,1980] [04,14,2010] 30
sample = Node "Will" [Node "Willow" [], Node "Jaden" []]
