module Find (
         Info(..),
         getInfo,
         traverse,
         find,
         foldTree
        ) where

import System.Directory (getDirectoryContents, getPermissions, Permissions(searchable), getModificationTime)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM, liftM, filterM)
import System.IO
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Control.Exception (handle, bracket, SomeException(..))

--use searchable to judge directory
data Info = Info {
                   name :: String,
                   isDir :: Bool,
                   size :: Integer,
                   modifyTime :: POSIXTime
                 } deriving (Show, Eq)

--write predicate for dsl
--define a INFOP type
--and play around for a while
type InfoP a = Info -> a

--get size
sizeP :: InfoP Integer
sizeP = size 

--get extension 
extensionP :: InfoP String
extensionP = takeExtension . name

--is searchable
searchableP :: InfoP Bool
searchableP = isDir

--modified time 
timeP :: InfoP POSIXTime 
timeP = modifyTime

equalP :: (Eq a) => (InfoP a) -> a -> InfoP Bool
equalP f o  = (\info -> f info == o)

liftP :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP f g m = (\info -> g info `f` m info)

greaterP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP p x = liftP (>) p (\_ -> x)

lessP :: (Ord a) => InfoP a -> a -> InfoP Bool
lessP p x = liftP (<) p (\_ -> x)

--and define new operator
(>?) :: (Ord a) => InfoP a  -> a -> InfoP Bool
(>?) = greaterP
infix 4 >?

andP :: (Eq a) => InfoP a -> InfoP a -> InfoP Bool
andP = liftP (==)

--so I can try to use andP to implement equalP
--brilliant
equalP' ::  (Eq a) => (InfoP a) -> a -> InfoP Bool
equalP' f info = andP f (\_ -> info) 

--and can write some filter function according to this
--file with extension .hs and 
predicate1 :: InfoP Bool
predicate1 = extensionP `equalP` ".txt"

--file size larger than 1000
predicate2 :: InfoP Bool
predicate2 = sizeP `lessP` (1000 :: Integer)

--combine predicate1 and predicate2
predicate3 :: InfoP Bool
predicate3 = predicate1 `andP` predicate2

--make sure exception type is given
getFileSize :: String -> IO Integer
getFileSize f = handle handler $ bracket (openFile f ReadMode) hClose hFileSize
                where handler :: SomeException -> IO Integer
                      handler _ = return (-1)


getInfo :: String -> IO Info
getInfo dir = do size <- getFileSize dir
                 time <- getModificationTime dir >>= return . utcTimeToPOSIXSeconds
                 isDir <- getPermissions dir >>= return . searchable
                 return $ Info dir isDir size time

isSearchable :: String -> IO Bool
isSearchable dir = getPermissions dir >>= return . searchable

--write a simple traverse to feel different types of traverse
--didn't not handle getDirectoryContents
traverseStr :: ([String] -> [String]) -> String -> IO [String]
traverseStr order dir = do contents <- getDirectoryContents dir
                           let valids = filter (`notElem` [".", ".."]) contents
                           liftM concat $ forM (order valids) $ \son_dir ->
                                        do let d = dir </>  son_dir
                                           flag <- isSearchable d
                                           if flag then traverseStr order d
                                                   else return [d]

--maybe I shouldn't use a [String] -> [String] type order function, should use [Info] -> [Info]
type OrderFunc = [String] -> [String]
traverse :: OrderFunc -> Info -> IO [Info]
traverse order info = do contents1 <- getUsefulContents info
                         let current_dir = name info
                         let contents = current_dir : map (current_dir </>) contents1
                         liftM concat $ forM (order contents) $ \son_dir ->
                                        do if son_dir == current_dir then return info >>= (\x -> return [x])
                                                                     else do flag <- isSearchable son_dir
                                                                             if flag then getInfo son_dir >>= traverse order
                                                                                     else getInfo son_dir >>= (\x -> return [x])

getUsefulContents :: Info -> IO [String]
getUsefulContents info = return (name info) >>= getDirectoryContents >>= return . filter (`notElem` [".", ".."])

--use utility like fold to implement traverse
--if take only 3 of the result, how to stop?
--what about add a predicate?
--add a iterate status

--package ret and status info
data Iterate seed = DONE { unwrap :: seed} |
                    SKIP { unwrap :: seed} |
                    CONTINUE { unwrap :: seed} deriving (Eq)

--now use a Iterator seed abstracts iterate a info and return ret and status
type Iterator seed = seed -> Info -> Iterate seed

--foldTree which take an iterator, an initial seed, an initial info 
--and return seed whenever Iterate seed implies Done
--but how can I modify traversal way?
foldTree :: (Show seed) => Iterator seed -> seed -> String -> IO seed
foldTree f seed dir = do info <- getInfo dir
                         foldTree' f seed [info]

foldTree' :: (Show seed) => Iterator seed -> seed -> [Info] -> IO seed 
foldTree' f seed [] = return seed
foldTree' f seed (info:infos) = do case f seed info of
                                        DONE seed' -> return seed'
                                        SKIP _ -> foldTree' f seed infos
                                        CONTINUE seed' -> if isDir info then 
                                                                        do contents1 <- getUsefulContents info
                                                                           contents <- sequence $ map (getInfo . ((name info) </>)) contents1
                                                                           foldTree' f seed' (contents ++ infos)
                                                                        else foldTree' f seed' infos

--and write a Iterator seed to test foldTree
--use predicate3 and judge the ret number exceeds 3 or not
p3Iter :: Iterator [Info]
p3Iter infos info = if predicate3 info then CONTINUE (info:infos)
                                       else CONTINUE infos

p3SizeIter :: Iterator [Info]
p3SizeIter infos info = if predicate3 info then if length infos == 2 then DONE (info: infos)
                                                                     else CONTINUE (info : infos)
                                           else CONTINUE infos


p3Skip :: Iterator [Info]
p3Skip infos info = if name info == "dir/b/bb" then SKIP infos
                                               else p3Iter infos info

--seems all done
type ConstructController seed info = seed -> info -> Iterate seed
type ValueController seed info = seed -> info -> seed


iterator :: ConstructController seed info -> ValueController seed info -> Iterator seed
iterator = undefined

find :: String -> IO [String]
find dir = do info <- getInfo dir
              traverse id info >>= filterM (return . predicate3) >>= return . map name 


main = do foldTree p3Iter [] "dir" >>= return . (map name) >>= print
          foldTree p3SizeIter [] "dir" >>= return . (map name) >>= print
          foldTree p3Skip [] "dir" >>= return . (map name) >>= print
