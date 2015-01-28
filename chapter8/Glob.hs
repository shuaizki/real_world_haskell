--filename expansion
import qualified  System.FilePath as FP 
import qualified System.Directory as Dir
import Control.Monad (forM)
import Data.List (concat, intercalate)
import GlobRegex (globMatched)
import System.IO 
import System.Console.Terminal.Size
import System.Environment (getArgs)

isPattern :: String -> Bool
isPattern = any (`elem` "*[!")


--find all possible pathes first
--then globMatch the file
--now still without exception handle module
--forM (a->m b) -> [a] -> m [b]

match :: String -> String -> Bool
match source pattern = globMatched source pattern True 
splitFileName :: String -> (String, String)
splitFileName name = (FP.dropTrailingPathSeparator dir, filename)
                        where (dir, filename) = FP.splitFileName name

glob :: String -> IO [String]
glob pat
        | not (isPattern pat) = do expand pat >>= nameExists >>= (\exist -> if exist then return [pat] else return [])
        | otherwise = do pattern <- expand pat
                         let path@(dirName, fileName) = splitFileName pattern
                         case isPattern dirName of
                                True -> do 
                                           dirs <- glob dirName
                                           let dirs1 = map (\a -> FP.joinPath [a, fileName]) dirs
                                           ret <- forM dirs1 glob
                                           return $ concat ret
                                False -> do Dir.doesDirectoryExist dirName >>= (\x -> if x then Dir.getDirectoryContents dirName >>= return . map (\a -> FP.joinPath [dirName, a]) . filter (\x -> match x fileName) 
                                                                                else return [] )

getRelative :: String -> IO String
getRelative s = Dir.getCurrentDirectory >>= return . (\current -> FP.makeRelative current s)

--getDirectoryContents
--used to expand directory like ~ or not absolute
expand :: String -> IO String
expand s  
          | FP.isAbsolute s = return s
          | otherwise = case s of
                        ('~':xs) -> join Dir.getHomeDirectory xs
                        others -> join Dir.getCurrentDirectory others
                        where join dirFunc xs = do dir <- dirFunc
                                                   let joined = FP.joinPath [dir, FP.dropDrive xs]
                                                   return joined

nameExists :: String -> IO Bool
nameExists path = do file <- Dir.doesFileExist path 
                     dir <- Dir.doesDirectoryExist path
                     return $ file || dir

--write a pretty intercalate to fit terminal size, and use my own glob insteam of ls
--okay with prettyIntercalate, but always print absolute path is a aweful exprience
--and finished this feature using makeRelative
tSize :: IO (Maybe Int)
tSize = size >>= return . (\x -> if x == Nothing then Nothing else (x >>=  Just . (fromInteger :: Integer -> Int) . width))

prettyIntercalate :: String -> [String] -> IO String
prettyIntercalate separator l = do width <- tSize
                                   case width of
                                        Just w  -> return (pretty' l 0)
                                                where pretty' [] c = ""
                                                      pretty' (x:xs) c  | c + (length x) + slen <= w = x++separator++pretty' xs (c + length x + slen)
                                                                        | c == 0 && length x + slen > w = x ++ "\n" ++ pretty' xs 0
                                                                        | otherwise = "\n" ++ pretty' (x:xs) 0
                                                      slen = length separator
                                        Nothing -> return $ intercalate separator l

glob' :: String -> IO ()
glob' x = return x >>= glob >>= sequence . (map  getRelative) >>= prettyIntercalate "  " >>= putStrLn


--test expand
expand1 = [
            expand "regex.hs",
            expand "~/dev/workspace/regex.hs",
            expand "/Users/shuaizki/dev/haskell/refamiliar/real_world_haskell/chapter8"
          ]

test_expand1 = do print "test expand1"
                  sequence expand1
--test nameExists true, false, true
exists1 = [
                "~/dev/haskell/refamiliar/real_world_haskell/chapter8/array.hs",
                "~/dev/haskell/refamiliar/real_world_haskell/chapter8/blahblah",
                "~/dev/haskell/refamiliar/real_world_haskell/chapter8"
          ]

test_exists1 = do print "test nameExist"
                  sequence ( map (\x -> expand x >>= nameExists) exists1)
--test glob
globs = [
               "*.hs",
                "~/dev/haskell/refamiliar/real_world_haskell/chapter8/regex.hs",
                "~/dev/haskell/refamiliar/real_world_haskell/chapter8/rege[a-z].hs",
                "~/dev/haskell/refamiliar/real_world_haskell/chapter8/rege[!a-w].hs",
                "~/dev/*/refamiliar/real_world_haskell/chapter8/regex.hs",
                "~/dev/*/refamiliar/*_world*/chapter8/regex.hs",
                "~/dev/haskel[!a-z]/refamiliar/*_world*/chapter8/regex.hs",
                "*.hi",
                "*.*",
                "a*/b*"
        ]

globs1 = [
                "~/dev/*/refamiliar"
         ]

test_globs = do print "test glob"
                sequence_ $ map (\x -> glob x >>= print) globs

--should use it like runhaskell "*.hs" to avoid shell expand
--and suddenly I realized my glob is an expanding utility instead of a ls utility
--fuck...I'll fix it later
--can use this making a rename files module
--also filter . and ..
main = do args <- getArgs
          print args
          let input = if length args == 0 then "*" else args !! 0
          return input >>= glob'
