import Data.List
toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x > 0 = let q = x `div` 10
                            r = x `rem` 10
                        in
                          r:(toDigitsRev q)
toDigitsRev _ = []

rev [] = []
rev (x:xs) = (rev xs) ++ [x]
 
toDigits :: Integer -> [Integer]
toDigits x = rev (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = let
                         doubleEveryOther' (y1:y2:ys) = y1:(2*y2):(doubleEveryOther' ys)
                         doubleEveryOther' ys       = ys
                      in
                         rev (doubleEveryOther' (rev xs))

sumDigits :: [Integer] -> Integer
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs
sumDigits _      = 0

validate :: Integer -> Bool
validate x = 0 == (sumDigits (doubleEveryOther (toDigits x))) `rem` 10

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c | n > 1 = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)
hanoi 1 a b c = [(a,b)]
hanoi _ _ _ _ = []

hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 n a b c d | n > 1 = let
                             x = n `div` 2
                             y = n - x
                           in
                             (hanoi x a c d) ++ (hanoi y a b d) ++ (hanoi x c b a)
hanoi2 1 a b _ _ = [(a,b)]
hanoi2 _ _ _ _ _ = []

compareLen xs ys = compare (length xs) (length ys)
sortListsByLen l = sortBy compareLen l

myIntersperse :: a -> [[a]] -> [a]
myIntersperse x (y1:y2:ys) = y1 ++ [x] ++ myIntersperse x (y2:ys)
myIntersperse _ [y] = y
myIntersperse _ [] = []

data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Show)

treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)
