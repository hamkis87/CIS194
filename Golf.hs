module Golf where

import Data.List

skips :: [a] -> [[a]]
skips l = zipWith g (replicate (length l) l) [1,2..(length l)]
   where   g k x = map (k!!) [x-1,2*x-1..(length k)-1]

localMaxima :: [Integer] -> [Integer]
localMaxima l = 
   fst (unzip (filter p (zip l (zipWith (&&) (zipWith (>) l ((head l):(init l)))  (zipWith (>) l (tail l ++ [last l]))))))
   where   p (_,b) = b
            
histogram :: [Integer] -> String
histogram zs = histogram' ws "==========\n0123456789\n"
   where   ws = countOccurences 0 0 (sort zs)
           
           countOccurences 10 _ _ = []
           countOccurences b c bs =
                        case (elemIndex b bs) of
                             Nothing -> c:(countOccurences (b+1) 0 bs)
                             Just _  -> countOccurences b (c+1) (tail bs)

           histogram' :: [Integer] -> String -> String
           histogram' xs s | sum xs == 0 = s
           histogram' xs s = histogram' xs' s'
               where xs' = map f xs
                     s'  = (map g xs) ++ "\n" ++ s
                     f 0 = 0
                     f a = a - 1
                     g 0 = ' '
                     g b = '*'
