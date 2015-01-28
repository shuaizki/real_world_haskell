
data CannotShow = CannotShow 
                deriving (Show)

data CannotDeriveShow = CannotDeriveShow CannotShow 
                        deriving (Show)

main = putStrLn (show $ CannotDeriveShow CannotShow)
