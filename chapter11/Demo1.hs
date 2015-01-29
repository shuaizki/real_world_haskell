import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.List

quickSort :: (Ord a) => [a] -> [a]
quickSort [] =  []
quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
        where left = [a | a <- xs, a <= x ]
              right = [a | a <- xs, a > x]

prop_idempotent :: [Int] -> Bool
prop_idempotent input = quickSort input' == input'
        where input' = quickSort input

--other property set
main = do verboseCheck prop_idempotent
