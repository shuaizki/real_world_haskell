--exercise 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

--exercise 2
splitWith :: (a->Bool) -> [a] -> [[a]]
splitWith f xs  = x1 : case x2 of
                        [] -> []
                        (a:b) -> [a] : splitWith f b
                where (x1, x2) = span f xs
--test
main = do
        print (safeHead [] :: Maybe Int)
        print (safeHead [1,2])
        print (safeTail [] :: Maybe [Int])
        print (safeTail [1])
        print (safeTail [1,2])
        print (splitWith (\x -> even x) [2,2,2,3,2,3,2,2,2])
