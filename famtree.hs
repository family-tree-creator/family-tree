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
	| c == (list:_) = Node p list

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

