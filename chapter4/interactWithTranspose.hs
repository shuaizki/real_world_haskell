import System.Environment

interactWith :: String -> String -> (String->String) -> IO ()
interactWith inputFile outputFile function = do input <- readFile inputFile 
                                                writeFile outputFile (function input)


main = do 
          args <- getArgs
          case args of 
                [inputFile, outputFile] -> interactWith inputFile outputFile function
                _ -> return (error "must give 2 arguments")

--transpose, use some stupid implementation

appendChar :: String -> Char -> String 
appendChar [] a = [a]
appendChar b a = b ++ [a]

append :: [String] -> String -> [String]
append = zipWith appendChar

helper = foldl append (repeat "")

a = "hello\nworld"

function :: String -> String
function = unlines . helper . lines
