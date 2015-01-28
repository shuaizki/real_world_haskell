--write some code to help thinking
import Data.ByteString.Lazy (ByteString(..), hGet, length, unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Int
import System.IO
import Data.Bits

--parsestate stands for the parsing phase now
data ParseState = ParseState {
                        string :: ByteString,
                        offset :: Int64
                    } deriving (Eq, Show)

--now define Parse a 
--type a is result type
--Parse stands for a function which takes a ParseState and return result and refreshed ParseState
--result is for return, and ParseState is for future use
newtype Parse a = Parse {
                        runParse :: ParseState -> Either String (a, ParseState)
                  } 

--write identity parser which takes a parsestate and return exactly the same state?
--what about use ParseState to stand for the parse result
--input staies the same, parseState staies the same
identity :: a -> Parse a
identity a = Parse (\x -> Right (a, x))

--then considering how to make a parse given a parser, a specified state and output result Either String a
parse :: Parse a -> ParseState -> Either String a
parse parser state = case runParse parser state of
                        Left err -> Left err
                        Right (a, state) -> Right a

idx_version = LC.pack "Lucene46SegmentInfo"
idx_length = fromIntegral $ Data.ByteString.Lazy.length idx_version :: Int
version_start = 0
version_checksum = 1

toInt :: (Integral a) => a -> Int
toInt = fromIntegral

readInt :: Handle -> IO Int
readInt h = do hGet h 4 >>= return . (\x -> zip (unpack x) [24, 16, 8, 0]) >>= return . sum . processed
                where processed = map (\(w, shift) -> (fromIntegral w :: Int) `shiftL` shift) 

readString :: Handle -> IO String
readString h = do i <- readVInt h
                  hGet h i >>= return . LC.unpack

--didn't check error here
readVInt :: Handle -> IO Int
readVInt h = readVInt' h 0
        where readVInt' h pos =  do n <- hGet h 1 >>= return . unpack >>= return . toInt . (!!0)
                                    if n > 0 
                                        then return $ toInt n 
                                        else (readVInt' h (pos + 1)) >>= (\x -> return $ x `shiftL` 7 + (n .&. andNumber))
                                    where andNumber  
                                                | pos <=2 = 0x7f
                                                | otherwise = 0xff

--and then think about how to chain parse and ignore pattern match in the middle of the parsing
--try to parsing lucene segment header
main = do f <- openFile "some.si" ReadMode
          readInt f >>= print
          codec <- readString f
          print codec
          actual_version <- readInt f
          print actual_version
          version <- readString f
          print version
          --well done, got segment header
