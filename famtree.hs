--
-- Famtree.hs
--

--tree in three forms: empty, single individual, married w/ children
data FamTree a = Empty | Node a [FamTree a] 
  deriving (Read, Eq)

instance Show (FamTree a) where
	show (Empty) = "Empty Tree"
--	show (Node a (list)) = a
	
--datatype to hold information about individual
data Individual = Individual { fName :: String
						     , lName :: String
						     , gender :: Char
						     , birth :: [Int]
						     , death :: [Int]
						     , age :: Int
						   } deriving (Read, Eq)

--displays Individual's information in a easier to read format
instance Show Individual where
	show (Individual f l g b d a) = f ++ ", " ++ l ++ ". Gender: " ++ show g ++ ". Born: " ++ show b ++ " Died: " ++ show d ++ ". Age: " ++ show a ++ "."
	 

--tree for single individual
singleTree :: a -> FamTree a
singleTree a = Node a []

--data FamTree a = Empty | Node {
--		root :: a,
--		children :: FamTree a 
--		} deriving (Show, Read, Eq) 

-- add child to parent: (delete?)
-- child added can just be String. 
-- Works for Individual, but then added children cannot have children of their own
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
	
--add version 3 for accepting Node w/ Individual
addChild'' :: FamTree Individual -> FamTree Individual -> FamTree Individual
addChild'' (Node (Individual {fName = f,lName = l, gender = g, birth = b, death = d, age = a}) iList) Empty = Empty
addChild'' (Node (Individual {fName = f,lName = l, gender = g, birth = b, death = d, age = a}) iList) (Node (Individual {fName = pf,lName = pl, gender= pg, birth = pb, death = pd, age = pa}) pList)
	| f == pf && l == pl = (Node (Individual {fName = pf,lName = pl, gender = pg, birth = pb, death = pd, age = pa}) pList) 
	| pList == [] = (Node (Individual {fName = pf,lName = pl, gender = pg, birth = pb, death = pd, age = pa}) (Node (Individual {fName = f,lName = l, birth = b, gender = g, death = d, age = a}) iList:[]) )
	| (Individual {fName = f,lName = l, gender = g,  birth = b, death = d, age = a}) `famElem` pList = (Node (Individual {fName = pf,lName = pl, gender = pg, birth = pb, death = pd, age = pa}) pList)
	| otherwise = Node (Individual {fName = pf,lName = pl, gender = pg, birth = pb, death = pd, age = pa}) (pList ++ [Node (Individual {fName = f,lName = l, gender = g, birth = b, death = d, age = a}) iList])

--add parent to child. 
-- parent added can be Individual, or String
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


--prints curr or root of tree
printRoot :: FamTree a -> FamTree a 
printRoot Empty = error "need tree to print root"
printRoot (Node x list) = Node x []

--prints just name of root 
printRoot' :: (Show a) => FamTree a -> String 
printRoot' Empty = error "need tree to print root"
printRoot' (Node x list) = show x

--prints Node if found in tree
-- still need to add return value for when not found/ bottom out
printFind :: (Eq a) => a -> FamTree a -> FamTree a
printFind a Empty = error "need tree to search"
printFind a (Node r (x:xs))
	| a == r = Node a []
	| Node a [] == x = Node a [] 
	| a `famElem` xs = singleTree a
	| otherwise = printFind a x  

--version 2. search with first and last name, prints Node if found
printFind' :: String -> String -> FamTree Individual -> FamTree Individual
printFind' fst lst Empty = error "need tree to search"
printFind' fst lst (Node (Individual {fName = f,lName = l, gender = g, birth = b, death = d, age = a}) (x:xs))
	| f == fst = Node (Individual {fName = f,lName = l, gender = g, birth = b, death = d, age = a}) []

--number of nodes in tree
-- doesn't do through lists inside list/ doesn't work yet
famTreeSize :: (Eq a) => FamTree a -> Int
famTreeSize Empty = 0
famTreeSize (Node x list) 
	| list == [] = 1
	| otherwise = 1 + length list 
	
--test variables
a = Node "Ann" []
b = Individual "Ann" "Kale" 'f' [1,10,1970] [1,10,2000] 30
c = Individual "Ann" "Whale" 'f' [2,20,1980] [5,20,1981] 1
d = Individual "person" "Whal" 'm' [0,0,0000] [1,1,1111] 70
sample = Node "Will" [Node "Willow" [], Node "Jaden" [Node "Child" []]]
