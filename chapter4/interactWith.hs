import System.Environment

interactWith :: String -> String -> (String->String) -> IO ()
interactWith inputFile outputFile function = do input <- readFile inputFile 
                                                writeFile outputFile (function input)


main = do 
          args <- getArgs
          case args of 
                [inputFile, outputFile] -> interactWith inputFile outputFile function
                _ -> return (error "must give 2 arguments")

function :: String -> String
function = unlines . (map take1) . lines 
                where take1 [] = []
                      take1 xs = take 1 xs
