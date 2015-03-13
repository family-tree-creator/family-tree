--
-- famtree.hs
--

import Data.Maybe
import Data.Char

type First  = String
type Last   = String
type Gender = Char
type Birth  = (Int, Int, Int)
type Death  = (Int, Int, Int)
type Age    = Int

-- an Individual 
type Name = (String, String)
data Info = Person Name Gender Birth Death Age deriving (Show, Read, Eq)


--tree in three forms: empty, single individual, married w/ children
data FamTree = Real Info [FamTree]
             | Temp Name --[FamTree] add if needed
             | Hold Name [Name] --FTCrumb? or list of names?
  deriving (Read, Eq)

instance Show FamTree where
    show (Real (Person (first, last) gen (bm,bd,by) (dm,dd,dy) age) _) = first ++ " " ++ last ++ " " ++ show gen ++ " age: " ++ show age
    show (Temp (first, last)) = first ++ " " ++ last
    show (Hold (first, last) _) = first ++ " " ++ last

-- left: previous nodes info, right: path back up
data FTCrumb = FTCrumb Info [FamTree] [FamTree]-- deriving (Show)

type FTZipper = (FamTree, [FTCrumb])

--instance Show FTZipper where
--    show (_, _) = "hello"

ftUp :: FTZipper -> FTZipper
ftUp (famTree, FTCrumb name prev crumb:crumbs) = (Real name (prev ++ [famTree] ++ crumb), crumbs)
--ftUP Nothing = Nothing

ftTo :: Name -> Maybe FTZipper -> Maybe (FTZipper)
ftTo lname (Just (Real info links, crumbs)) =
    let (ls, rs) = break (nameCheck lname) links
    --in (head rs, FTCrumb info ls (tail rs):crumbs)
    in if rs == []
       then Nothing
       else Just (head rs, FTCrumb info ls (tail rs):crumbs)
ftTo _ _ = Nothing

nameCheck :: Name -> FamTree -> Bool
nameCheck (first, last) (Real (Person (rFirst, rLast) _ _ _ _) _) = (first == rFirst) && (last == rLast)
nameCheck (first, last) (Temp (tFirst, tLast)) = (first == tFirst) && (last == tLast)
nameCheck (first, last) (Hold (hFirst, hLast) _) = (first == hFirst) && (last == hLast)
--nameCheck _ _ = False

ftSearch :: Name -> Char -> Maybe FTZipper -> Maybe (FTZipper)
ftSearch name pType p@(Just (person@(Real _ _), crumbs)) =
    if (nameCheck name person)  && (checkType pType p)
    then p
    else ftSearchR name pType p --call another function
ftSearch name pType p@(Just (person@(Temp _), _)) =
    if (nameCheck name person) && (checkType pType p)
    then p
    else Nothing
ftSearch name pType p@(Just (person@(Hold _ _), _)) =
    if (nameCheck name person) && (checkType pType p)
    then p
    else Nothing
ftSearch _ _ _ = Nothing
--ftSearch (first, last) (Temp (tFirst, tLast), crumbs) =

ftSearchR :: Name -> Char -> Maybe FTZipper -> Maybe FTZipper --may be able to remove in later fix
ftSearchR name pType p@(Just (Real info links, crumbs)) = --Just (p)
    let r =  ftTo name p
    in if (Data.Maybe.isJust r) && (checkType pType r)
       then r
       else ftSearchN name pType p 0-- map to links?
ftSearchR name pType p@(Just (person@(Temp _), _)) = 
    if (nameCheck name person) && (checkType pType p)
    then p
    else Nothing
ftSearchR name pType p@(Just (person@(Hold _ _), _)) =
    if (nameCheck name person) && (checkType pType p)
    then p
    else Nothing
ftSearchR _ _ _ = Nothing

ftSearchN :: Name -> Char -> Maybe FTZipper -> Int -> Maybe (FTZipper)
ftSearchN name pType p@(Just (Real info links, crumbs)) place = 
    let (ls, rs) = splitAt place links
    in if rs == []
       then Nothing
       else let r = ftSearch name pType (Just (head rs, FTCrumb info ls (tail rs):crumbs))
            in if (Data.Maybe.isJust r) && (checkType pType r)
               then r
               else ftSearchN name pType p (place + 1)
ftSearchN name pType p@(Just (person@(Temp _), _)) _ =
    if (nameCheck name person) && (checkType pType p)
    then p
    else Nothing
ftSearchN name pType p@(Just (person@(Hold _ _), _)) _ =
    if (nameCheck name person) && (checkType pType p)
    then p
    else Nothing
ftSearchN _ _ _ _ = Nothing

replaceTemp :: FamTree -> Maybe FTZipper -> Maybe FTZipper -- fix
replaceTemp (Real info _) (Just (Real _ links, crumbs)) = Just (Real info links, crumbs)
replaceTemp (Real info _) (Just (Temp _, crumbs)) = Just (Real info [], crumbs)
replaceTemp _ Nothing = Nothing

insertPerson :: FamTree -> Maybe FTZipper -> Maybe FTZipper
insertPerson person (Just (Real info links, crumbs)) = Just (Real info (person:links), crumbs)
insertPerson _ Nothing = Nothing

checkType :: Char -> Maybe FTZipper -> Bool
checkType pType (Just (Real _ _, _)) = pType == 'r' -- ||
checkType pType (Just (Temp _, _)) = pType == 't' -- ||
checkType pType (Just (Hold _ _, _)) = pType == 'h' -- || pType == 'r'
checkType _ _ = False

printFamTree :: Maybe FTZipper -> String
printFamTree (Just (p@(Real _ ls), _)) = "Father: " ++ (show (last ls)) ++ ", Mother: " ++ (show (last (init ls))) ++ "\n" ++
                                       (show p) ++ "\n" ++
                                       "Children: " ++ (printChildren ls)
printFamTree (Just (p@(Temp _), _)) = (show p)
printFamTree (Just (p@(Hold _ _), _)) = (show p)
printFamTree Nothing = ""

printChildren :: [FamTree] -> String
printChildren (person:ls) = if (length ls) > 1
                          then (show person) ++ (printChildren ls)
                          else ""

makePerson :: [String] -> [FamTree]
makePerson (first:ls) = if (length (first:ls)) > 10
                        then placeGender (Real (Person (first, (head ls)) ' ' (0,0,0) (0,0,0) 0) []) (tail ls)
                        else error "Not enough info to make a FamTree"

placeGender :: FamTree -> [String] -> [FamTree]
placeGender (Real (Person name _ b d a) rel) (gen:ls) = let gender = head gen
                                                     in if (gender == 'm') || (gender == 'f')
                                                        then placeBirth (Real (Person name gender b d a) rel) ls
                                                        else error "Not correct gender"

placeBirth :: FamTree -> [String] -> [FamTree]
placeBirth (Real (Person n g _ d a) rel) (bir:ls) = placeDeath (Real (Person n g (calculateDate bir) d a) rel) ls

placeDeath :: FamTree -> [String] -> [FamTree]
placeDeath (Real (Person n g b _ a) rel) (dea:ls) = [(Real (Person n g b (calculateDate dea) a) rel)] -- ls

calculateDate :: String -> (Int,Int,Int)
calculateDate date = (0,0,0)

splitDate :: String -> [String]
splitDate [] = [""]
splitDate (x:xs)
    | isDigit x = (x : head mid) : tail mid
    | otherwise = "": mid
    where
        mid = splitDate xs

--ftParrent
--ftChild

x -: f = f x 

testTree :: FamTree
testTree = 
    Real (Person ("Chris", "Peck") 'm' (03,28,1994) (0,0,0) 21)
        [ Real (Person ("Judith", "Peck") 'f' (8,9,0) (1,2,3) 4)
            [ Temp ("Mary", "Mauck")
            , Temp ("Andrew", "Mauck")
            ]
        , Real (Person ("Charlie", "Peck") 'm' (1,2,3) (4,5,6) 7) 
            [ Hold ("Chris", "Peck") []
            , Temp ("Larraine", "Peck")
            , Temp ("Charles", "Peck")
            ]
        ]