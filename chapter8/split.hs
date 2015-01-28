import qualified Data.ByteString.Lazy as L
import qualified Data.List.Split as S
import qualified Data.ByteString.Lazy.Builder as LB

bs = LB.toLazyByteString $ LB.string8 "oiaodsf->oiajdofj"
pattern = LB.toLazyByteString $ LB.string8 "->"

main = do print (S.splitOn "->" "981029->91809328")
