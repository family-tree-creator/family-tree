--
-- Famtree.hs
--

--tree in three forms: empty, single individual, married w/ children
data FamTree a = Empty | Node [FamTree a]
  deriving (Show, Read, Eq)


data Individual = Individual { firstName :: String
						     , lastName :: String
						     , birth :: [Int]
						     , death :: [Int]
						     , age :: Int
						   } deriving (Show)
						   
printIndividual :: Individual -> String
printIndividual (Individual {firstName = f,lastName = l, birth = b, death = d, age = a}) = f ++
     " , " ++ l ++ ". Birth date: " ++ show b ++ " Death date: " ++ show d ++" Age: " ++ show a
 
--error: unexpected kind variable 'a'.  
--data Node :: FamTree a -> FamTree a -> FamTree a 
 
--prints curr or root of tree
--toList :: famTree a -> Node 
--toList (Empty) = Empty
--toList (Node) = Node
--toList (Node (xs)) = Node 

--sample = Node "Will" [Node "Willow" [], Node "Jaden" []]

