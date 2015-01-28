import Data.Array
import Data.Ix

--Data.Ix is used to map a continguous subrange of values onto integers. It's used primarily for array indexing.

--four methods supported for this typeclass
--range :: (a, a) -> [a]
num_range = range (1, 10)
char_range = range ('a', 'z')
tuple_range = range ((1, 1), (5, 5))

--index :: (a, a) -> a -> Int
num_idx = index (1, 10) 2
tuple_idx = index ((1, 1), (5, 5)) (2, 3)

--inRange :: (a, a)-> a-> Bool
num_in = inRange (1, 10) 2
tuple_in = inRange ((1, 1), (5, 5)) (2, 3)

--rangeSizea :: (a, a) -> Int
tuple_size = rangeSize ((1, 1), (5, 5))

--Data.Array
--construct
array1 = array (1, 10) [(1, "wakak"), (1, "blah"), (2, "hoho")]
--array is strict in the bounds argument and in the indices of the association list, but non-strict in the values, so the below is legal
array2 = array (1,100) ((1,1) : [(i, i * array2!(i-1)) | i <- [2..100]])

--listArray
array3 = listArray (1, 5) [2 * x | x <- [2..10]]

--accumArray
array4 = accumArray (+) 2 (1, 10) ((2,3):[(i, i)| i <- [1..10]])
l1 = [(1,2), (1,3)] ++ [(2,2), (2, 3), (2, 4)] ++ [(i, 1) | i <- [1..10]]
array5 = accumArray (*) 1 (1, 10) l1

--accum
array_ = array  (1,3) [(1, 1), (1, 2), (1, 10), (2, 1), (3, 1)]
array6 = accum (+) array_ []
--use accum to implement accumArray
myAccumArray f ini r = accum f (array r [(i, ini) | i <- range r])

main = do print "range"
          print num_range
          print char_range
          print tuple_range
          print "index"
          print num_idx
          print tuple_idx
          print "inRange"
          print num_in
          print tuple_in
          print "rangeSize"
          print tuple_size
          print "array"
