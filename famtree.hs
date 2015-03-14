--
-- famtree.hs
--

module FamTree
( ftUp
, ftTo --make new one that deals with Holds
, ftSearch
, makeTree
, FTZipper
, printFamTree
) where

import Data.Maybe
import Data.Char
import Data.Time.Clock
import Data.Time.Calendar
import System.IO

type First  = String
type Last   = String
type Gender = Char
type Date   = (Int, Int, Int)
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
             | Empty
  deriving (Read, Eq)

instance Show FamTree where
    show (Real (Person (first, last) gen (bm,bd,by) (dm,dd,dy) age) _) = first ++ " " ++ last ++ " " ++ show gen ++ " age: " ++ show age
    show (Temp (first, last)) = first ++ " " ++ last
    show (Hold (first, last) _) = first ++ " " ++ last
    show Empty = "Empty"

-- left: previous nodes info, right: path back up
data FTCrumb = FTCrumb Info [FamTree] [FamTree] deriving (Show)

type FTZipper = (FamTree, [FTCrumb])

--instance Show FTZipper where
--    show (_, _) = "hello"

ftUp :: Maybe FTZipper -> Maybe FTZipper
ftUp (Just (famTree, FTCrumb name prev crumb:crumbs)) = (Just (Real name (prev ++ [famTree] ++ crumb), crumbs))
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
checkType pType (Just (Real _ _, _)) = (pType == 'r') || (pType == 'a')
checkType pType (Just (Temp _, _)) = (pType == 't') -- ||
checkType pType (Just (Hold _ _, _)) = (pType == 'h') || (pType == 'r') || (pType == 'a')
checkType _ _ = False

printFamTree :: Maybe FTZipper -> String
printFamTree (Just (p@(Real _ ls), _)) = "Father: " ++ (show (last ls)) ++ ", Mother: " ++ (show (last (init ls))) ++ "\n" ++
                                       (show p) ++ "\n" ++
                                       "Children: " ++ (printChildren ls)
printFamTree (Just (p@(Temp _), _)) = (show p)
printFamTree (Just (p@(Hold _ _), _)) = (show p)
printFamTree (Just (Empty, _)) = "Empty"
printFamTree Nothing = "Nothing"

printChildren :: [FamTree] -> String
printChildren (person:ls) = if (length ls) > 1
                          then (show person) ++ (printChildren ls)
                          else ""

makePerson :: [String] -> Date -> [FamTree]
makePerson (first:ls) da = if (length (first:ls)) > 10
                           then placeGender (Real (Person (first, (head ls)) ' ' (0,0,0) (0,0,0) 0) []) (tail ls) da
                           else []

placeGender :: FamTree -> [String] -> Date -> [FamTree]
placeGender (Real (Person name _ b d a) rel) (gen:ls) da = let gender = toLower (head gen)
                                                           in if (gender == 'm') || (gender == 'f')
                                                              then placeBirthDeath (Real (Person name gender b d a) rel) ls da
                                                              else error "Not correct gender"

placeBirthDeath :: FamTree -> [String] -> Date -> [FamTree]
placeBirthDeath (Real (Person n g _ d a) rel) (bir:(dea:ls)) da = placeAgeRelatives (Real (Person n g (calcDate bir) (calcDate dea) a) rel) ls da

placeAgeRelatives :: FamTree -> [String] -> Date -> [FamTree]
placeAgeRelatives (Real (Person n g b d a) rel) ls da = (Real (Person n g b d (calcAge b d da)) rel):(makeRelatives ls)

makeRelatives :: [String] -> [FamTree]
makeRelatives (ffn:(fln:(mfn:(mln:ls)))) = (makeChildren ls) ++ [(Temp (mfn,mln)), (Temp(ffn,fln))]

makeChildren :: [String] -> [FamTree]
makeChildren ("end":ls) = []
makeChildren (child:ls) = let name = break (==' ') child
                        in  (Temp name) : (makeChildren ls)

calcAge :: Date -> Date -> Date -> Int
calcAge (0,0,0) _ _ = 0
calcAge (bm, bd, by) (0,0,0) (cm, cd, cy) = if (bm > cm) || ((bm == cm) && (bd > cd))
                                            then cy - by - 1
                                            else cy - by
calcAge (bm, bd, by) (dm, dd, dy) _ = if (bm > dm) || ((bm == dm) && (bd > dd))
                                    then dy - by - 1
                                    else dy - by

calcDate :: String -> Date
calcDate ""   = (0,0,0)
calcDate date = let (m:(d:(y:da))) = splitDate date
                     in ((read m), (read d), (read y))

splitDate :: String -> [String]
splitDate [] = [""]
splitDate (x:xs)
    | isDigit x = (x : head mid) : tail mid
    | otherwise = "": mid
    where
        mid = splitDate xs

attachPerson :: [FamTree] -> Maybe FTZipper -> Maybe FTZipper
attachPerson ((Real info _):ls) (Just (Empty, [])) = Just ((Real info ls, [])) --addrelattives
attachPerson ((Real info@(Person name _ _ _ _) _):ls) z = let zip@(Just (per, crumbs)) =  ftSearch name 't' (goToRoot z) 
                                                       in if Data.Maybe.isJust zip
                                                          then makeHolds name (makeAddress zip) (Just ((Real info (checkTemps ls zip)), crumbs))
                                                          else zip
attachPerson [] zip = zip

makeHolds :: Name -> [Name] -> Maybe FTZipper -> Maybe FTZipper
makeHolds name ls z = let zip@(Just (per, crumbs)) =  ftSearch name 't' (goToRoot z) 
                      in if Data.Maybe.isJust zip
                         then makeHolds name ls (Just ((Hold name ls), crumbs))
                         else zip

checkTemps :: [FamTree] -> Maybe FTZipper -> [FamTree]
checkTemps [] _ = []
checkTemps (per@(Temp name):ls) z = let person = ftSearch name 'r' (goToRoot z)
                                   in if Data.Maybe.isNothing person
                                      then per:(checkTemps ls z)
                                      else if checkType 'h' person
                                           then (Hold name (getAddress person)) : (checkTemps ls z)
                                           else (Hold name (makeAddress person)) : (checkTemps ls z)
makeAddress :: Maybe FTZipper -> [Name]
makeAddress (Just (_, [])) = []
makeAddress (Just (p@(Real (Person name _ _ _ _) _), FTCrumb n prev crumb:crumbs)) = (makeAddress (Just (Real n (prev ++ [p] ++ crumb), crumbs))) ++ [name]

getAddress :: Maybe FTZipper -> [Name]
getAddress (Just ((Hold _ ls), _)) = ls

goToRoot :: Maybe FTZipper -> Maybe FTZipper
goToRoot Nothing = Nothing
goToRoot z@(Just (_, [])) = z
goToRoot z@(Just (_, crumbs)) = goToRoot(ftUp z)

makeEmptyTree :: Maybe FTZipper
makeEmptyTree = Just (Empty, [])

makeTree :: [[String]] -> Date -> Maybe FTZipper
makeTree ls date = createTree ls date makeEmptyTree

createTree :: [[String]] -> Date -> Maybe FTZipper -> Maybe FTZipper
createTree [] _ zip         = zip
createTree (info:ls) date zip = attachPerson (makePerson info date) zip

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