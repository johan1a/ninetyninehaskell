import Data.List

one :: [a] -> a
one = last

two :: [a] -> a
two = (!!1) . reverse

elementAt ::  [a] -> Int -> a
elementAt l k = l !! (k-1) 

myLength :: [a] -> Int
myLength = length

myReverse :: [a] -> [a]
myReverse = reverse

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == (myReverse list)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

flatten3 :: NestedList a -> [a]
flatten3 (Elem x) = [x]
flatten3 (List xs) = foldr1 (++) $ map flatten3 xs

compress :: Eq a => [a] -> [a]
compress = map head . group

pack :: Eq a => [a] -> [[a]]
pack = group

encode :: Eq a => [a] ->[(Int,a)]
encode = map (\t -> (length t,head t)) . pack

data Multiple a = Single a | Multiple Int a 
	deriving (Show)
	
encodeModified :: Eq a=> [a] -> [Multiple a]
encodeModified = map encodeModified' . encode 

encodeModified' (k,x) 
	| k == 1 = Single x
	| otherwise = Multiple k x 