import Data.List hiding (union)
import Data.List
import Data.Maybe

maxi :: Int -> Int -> Int
maxi x y
    | x > y = x
    | otherwise = y

sumsq :: Int -> Int
sumsq 0 = 0
sumsq nbr = 2 ^ nbr + sumsq (nbr - 1)


hanoi :: Int -> Int
hanoi n = 1 + 2 * hanoi (n-1)

smallestFactor :: Int -> Int
smallestFactor n = minimum  [x | x <- [2..n], mod n x  == 0]

numFactors :: Int -> Int
numFactors n = length [x | x <- [2..n], mod n x  == 0]

type Month = Int

daysInMonth :: Month -> Integer -> Integer
daysInMonth m y
  | m < 1 || m > 12   = 0
  | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12  = 31
  | m == 4 || m == 6 || m == 9 || m == 11   = 30
  | mod y 4 == 0 && m == 2 = 29
  | otherwise     = 28

data Date = Date Integer Month Integer

validDate :: Date -> Bool
validDate (Date year month day)
    |daysInMonth month year > day = True
    |otherwise  = False

multiply :: Num a => [a] -> a
multiply [] = 0
multiply xs = foldr (*) 1 xs

substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute this byThat (x:xs)
    | x == this = byThat : substitute this byThat xs
    | otherwise = x : substitute this byThat xs

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs)
    | x `elem` xs = True
    |otherwise = duplicates xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

--Returns all possible pairs of lists xs and ys
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x<-xs, y<-ys]

isPermutation :: Eq a => Ord a  => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

compareElements :: [a] -> [a] -> Ordering
compareElements list1 list2 = compare (length list1) (length list2)

sortByLength :: [String] -> [String]
sortByLength [] = []
sortByLength xs = sortBy compareElements xs

shortestAndLongest :: [String] -> (String,String)
shortestAndLongest xs = (head $ sortByLength xs, last $ sortByLength xs)


mystery :: [a] -> [a]
mystery xs = foldr (++) [] (map (\y -> [y]) xs)


data File = File String
    | Dir String [File] deriving (Eq, Show)

type FileSystem = [File]

search :: FileSystem -> String -> [String]
search files name =
  [ name | File name' <- files, name == name'] ++
  [ dir ++ "/" ++ path | Dir dir files' <- files, path <- search files' name]

data Proposition = Var Name
                 | Proposition :&: Proposition
                 | Proposition :|: Proposition
                 | Not Proposition
 deriving ( Eq, Show ) 

type Name = String

---

vars :: Proposition -> [Name]
vars (Var x)   = [x]
vars (a :&: b) = vars a `union` vars b
vars (a :|: b) = vars a `union` vars b
vars (Not a)   = vars a

truthValue :: [(Name,Bool)] -> Proposition -> Bool
truthValue val (Var x)   = fromJust (lookup x val)
truthValue val (a :&: b) = truthValue val a && truthValue val b
truthValue val (a :|: b) = truthValue val a || truthValue val b
truthValue val (Not a)   = not (truthValue val a)

---

-- allVals xs enumerates all possible valuations of the variables xs:
--    1. when xs = [], there is just one valuation
--    2. otherwise, we enumerate all possible valuations for the rest
--       of the variables, plus all possible values of x
allVals :: [Name] -> [[(Name,Bool)]]
allVals []     = [[]]
allVals (x:xs) = [ (x,b):val | val <- allVals xs , b <- [False,True] ]

tautology :: Proposition -> Bool
tautology a =
  and [ truthValue val a | val <- allVals (vars a) ]

-- x / (8 - y)
f x  = (flip(/))  ((-) 8 ) x


