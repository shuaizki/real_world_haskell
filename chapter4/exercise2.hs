--implement asInt
import Data.Char

--plain version
asInt :: String -> Int
asInt i = let (ret, flag) = foldl compute (0, True) i
          in if flag then ret else (-ret)
          where compute (acc, flag) '-' =  (-acc, False)
                compute (acc, flag) a = (acc * 10 + digitToInt a, flag)

--handle . error
asInt' :: String -> Integer
asInt' i = let (ret, flag) = foldl compute (0, True) i
          in case ((null i), flag) of
                (True, _) -> 0
                (_, True) -> ret
                (_, False) -> toInteger (-ret)

compute :: (Integer, Bool) -> Char -> (Integer, Bool)
compute _ '.' = error ". not digit"
compute (acc, flag) '-' =  (-acc, False)
compute (acc, flag) a = (acc * 10 + toInteger (digitToInt a), flag)

--handle error rightly
asInt'' :: String -> Either String Integer
asInt'' i = let (ret, flag) = foldl compute' (Right 0, True) i
          in case (ret, flag) of
                (Left a, _) -> Left a
                (Right x, True) -> Right x
                (Right x, False) ->  Right (-x)

compute' :: (Either String Integer, Bool) -> Char -> (Either String Integer, Bool)
compute' (Right acc, flag) x 
        | isNumber x = (Right (acc * 10 + toInteger (digitToInt x)), flag)
        | x == '-' = (Right (-acc), False) 
        | otherwise = (Left ("illegal " ++ [x]), flag)
compute' a x = a


--my own concat
myConcat' :: [[a]] -> [a]
myConcat' = foldr (\x acc -> x ++ acc) [] 

--my own takeWhile
myTakeWhile :: (a->Bool) -> [a] -> [a]
myTakeWhile func i = reverse $ snd $ (foldr (\x (flag, acc) -> case (flag, func x) of
                                                (True, a) -> (True, acc)
                                                (f, False) -> (True, acc)
                                                (f, True) -> (f, x:acc)
                         ) (False, [])  (reverse i))

--my groupBy
--myGroupBy :: (a -> a -> Bool) -> [a] -> [a]
myGroupBy func i = let (a, b, c) =  foldr (\x (prev_value, count, acc) -> case (prev_value, count, acc) of
                                                        (Nothing, count, acc) -> (Just x, count + 1, acc)
                                                        (Just c, count, acc) -> if not (func x c) then (Just x, 1, (take count $ repeat c):acc)
                                                                                                       else (Just c, count + 1, acc)
                                              ) (Nothing, 0, []) i
                   in case a of 
                        Nothing -> c
                        Just x -> (take b $ repeat x) : c


main = do print (asInt "101")
          print (asInt "-31337")
          print (asInt "1798")
          print (asInt' "-")
          print (asInt' "")
          print (asInt' "-3")
          print (asInt' "314159265358979323846")
          print (asInt'' "27.98")
          print (asInt'' "-")
          print (asInt'' "")
          print (asInt'' "314159265358979323846")
          print (asInt'' "-314159265358979323846")
          print (myTakeWhile even [2,4,2,8,5,3,2,2])
          print (myGroupBy (==) [2,2,2, 4,4, 2,2 ,8,5,3,2,2])

