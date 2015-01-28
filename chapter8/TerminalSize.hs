import System.Console.Terminal.Size

tSize :: IO (Maybe Integer)
tSize = size >>= return . (\x -> if x == Nothing then Nothing else (x >>=  Just . width))


