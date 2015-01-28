import Text.Regex.Posix ((=~))
import qualified Data.ByteString.Lazy as L
import qualified System.IO as IO
import qualified Text.Regex.Base.RegexLike as RL
import qualified Data.List
import qualified Data.ByteString.Lazy.Builder as LB

s2bs :: String -> L.ByteString
s2bs = LB.toLazyByteString . LB.string8

pattern = s2bs "[0-9]*K->[0-9]*K"
spliter = s2bs "->"

list2tuplelist :: [a] -> [(a, a)]
list2tuplelist (x:y:xs) = (x,y): list2tuplelist xs
list2tuplelist other = []

process :: L.ByteString -> [L.ByteString]
process s 
        | s == L.empty = []
        | otherwise = (split match) : process suf
                where (pre, match, suf) = myMatch s pattern

split :: L.ByteString -> L.ByteString
split s = let (x, y, z) =  myMatch s spliter in z
input = s2bs "0912908091->90109820"

myMatch :: L.ByteString->L.ByteString->(L.ByteString, L.ByteString, L.ByteString)
myMatch a b = a =~ b :: (L.ByteString, L.ByteString, L.ByteString)

--test process
input1 = s2bs "[GC [PSYoungGen: 571290K->97609K(579072K)] 2414318K->1941153K(3992576K), 0.0752550"
ret1 = myMatch input1  pattern 

main = do handle <- IO.openFile "gc.out" IO.ReadMode 
          contents <- L.hGetContents handle
          mapM_ print (list2tuplelist $ process contents)
