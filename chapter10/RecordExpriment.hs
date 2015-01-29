
data Demo = Demo {
                name :: String,
                age :: Int
                } deriving (Show)

main = do let demo1 = Demo {name = "shuaizki"}
              demo2 = demo1 {age = 25}
          print demo2
