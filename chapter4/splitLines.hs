--write myself to feel it

splitLines :: String -> [String]
splitLines [] = []
splitLines s = let (pre, after) = break (\x -> x== '\r' || x == '\n') s
               in pre : case after of
                                ('\r':'\n':rest) -> splitLines rest
                                ('\r':rest) -> splitLines rest
                                ('\n':rest) -> splitLines rest
                                _ -> []
