import Find
import Data.List (sortBy)

--exercise 1 sort by reverse alphabetic order
reverseAlphaBetic :: String -> String -> Ordering
reverseAlphaBetic x y | x > y = LT
                      | x == y = EQ
                      | otherwise = GT

reverseOrder = sortBy reverseAlphaBetic

--exercises 2
--see below

--exercises 3
--and exercises 4 WTF
main = do --in normal order
          print "in normal order"
          info <- getInfo "dir"
          traverse id info >>= print . map name
          print "in reverse alphabetic"
          traverse reverseOrder info >>= print . map name
          print "use find"
          find "dir"
