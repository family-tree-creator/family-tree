data famTree a = Empty | Node a | Node a [Tree a] 
  deriving (Show, Read, Eq)

--tree in three forms: empty, single individual, married w/ children

  
--sample :: Tree String
--sample = Node "Will" [Node "Willow" [], Node "Jaden" []]

--individual :: a -> Tree a
--individual x = Node x [Empty]

