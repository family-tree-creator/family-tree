--
-- famtree.hs
--

module Famtree
( FamTree
, Individual
, indName
, indName' 
, drawTree
, singleTree 
, addChild 
, addChild' 
, addChild''
, addParent
, addParent' 
, famElem
, printRoot
, printRoot' 
, printFind
, famTreeSize
, childSize
) where  

--tree in three forms: empty, single individual, married w/ children
data FamTree a = Empty | Node a [FamTree a] 
  deriving (Show, Read, Eq)

--instance Show (FamTree a) where
--	show (Empty) = "Empty Tree"
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

--function for displaying only first and last name
indName:: Individual -> String
indName (Individual {fName = f,lName = l, gender = g, birth = b, death = d, age = a}) = f ++ ", " ++ l

--version 2 indName, gets names from Node Individual
indName' :: FamTree Individual -> String
indName' Empty = ""
indName' (Node (Individual {fName = f,lName = l, gender = g, birth = b, death = d, age = a}) list) = f ++ ", " ++ l
	 
--draw tree
-- need more time to implement, can be scrapped for now
drawTree :: FamTree Individual -> IO()
drawTree Empty = error "can't draw Empty Tree"
drawTree (Node x list) = putStrLn ("    " ++ indName x  ++ "\n" ++
 " ------------------------" ++ "\n" ++
 "" ++ "/" ++ "          " ++ "|" ++ "             " ++ "\\" ++ "\n" ++
 indName'(head list) )

--tree for single individual
singleTree :: a -> FamTree a
singleTree a = Node a []


-- add child to parent: 
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
printRoot' :: (Show a) => FamTree a -> IO() 
printRoot' Empty = error "need tree to print root"
printRoot' (Node x list) = putStrLn(show x)

--prints Maybe Node if found in tree
printFind :: (Eq a) => a -> FamTree a -> Maybe (FamTree a)
printFind a Empty = Nothing
printFind a (Node r list)
    | a == r = Just (Node a [])
    | a `famElem` list = Just (singleTree a)
    | otherwise = printFind a x
    where x | list == [] = Empty | otherwise = head list

--navigate through tree
changeRoot :: (Eq a) => FamTree a -> FamTree a -> FamTree a
changeRoot (Node a aList) Empty = error "need tree to navigate" 
--changeRoot (Node a aList) (Node r list) = y
 --  where x = printFind a (Node r list) 
 --        y 
 --        | x == Just (Node a []) = (Node a aList)
 --        | otherwise = Nothing


-- number of children of one parent	
childSize :: (Eq a) => FamTree a -> Int
childSize Empty = error "cannot calculate children for Empty tree"
childSize (Node x list) 
	| list == [] = 0
	| otherwise = length list

--number of nodes in tree
-- doesn't go through lists inside list/ doesn't work yet
famTreeSize :: (Eq a) => FamTree a -> Int
famTreeSize Empty = 0
famTreeSize (Node x list) 
	| list == [] = 1
	| otherwise = length list + famTreeSize y  -- + every other child in list! 
	where y | list == [] = Empty | otherwise = head list

	
--test variables
a = Node "Ann" []
b = singleTree (Individual "Ann" "Kale" 'f' [1,10,1970] [1,10,2000] 30)
c = Individual "Ann" "Whale" 'f' [2,20,1980] [5,20,1981] 1
d = Individual "person" "Whal" 'm' [0,0,0000] [1,1,1111] 70
sample = Node "Will" [Node "Willow" [], Node "Jaden" [Node "Child" [Node "Dude" []]]]
