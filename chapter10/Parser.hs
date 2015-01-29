--write some code to help thinking
import Data.ByteString.Lazy (ByteString(..), hGet, length, unpack, pack, hGetContents)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as LM
import qualified Data.Set as S
import Data.Int
import System.IO
import Data.Bits
import Control.Arrow (second)

--parsestate stands for the parsing phase now
--and how to store a segmentInfo
--can use applicative to reconstruct

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


readString :: Handle -> IO String
readString h = do i <- readVInt h
                  hGet h i >>= return . LC.unpack


data SegmentInfo = SegmentInfo {
                        codecName :: String,
                        versionStart :: Int,
                        version :: String,
                        docCount :: Int,
                        isCompoundFile :: Bool,
                        diagnostics :: [(String, String)], 
                        files :: [String] 
                        } deriving (Show, Eq)

defaultSI = SegmentInfo "" (-1) "" (-1) False [] []

data ParseState = ParseState {
                        string :: ByteString
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

toInt :: (Integral a) => a -> Int
toInt = fromIntegral
--how to chain parse together ? use some thing like monad
--used to chain operation together... can give it a shot when checking header
(>==) :: Parse a -> Parse b -> Parse b
parseA  >== parseB = Parse (\state -> 
                        case (runParse parseA) state of
                                Left err -> Left err
                                Right (a, state1) -> (runParse parseB) state1)

--but how to execute according to the previous result
--takes the first parse result and some transform function transform to another parse
(>==?) :: Parse a -> (a -> Parse b) -> Parse b
parseA >==? genParse = Parse chained 
        where chained state = case runParse parseA state of
                                Left err -> Left err
                                Right (a, state1) -> (runParse (genParse a)) state1 

--define a function to store the current result into   
parseInt :: Parse Int
parseInt = Parse parse'
        where parse' state = Right (sum $ processed $  zip (unpack bs) [24, 16, 8, 0], ParseState rest)
                where bs = L.take 4 $ string state
                      processed = map (\(w, shift) -> toInt w  `shiftL` shift)
                      rest = L.drop 4 $ string state

readInt :: Handle -> IO Int
readInt h = do hGet h 4 >>= return . (\x -> zip (unpack x) [24, 16, 8, 0]) >>= return . sum . processed
                where processed = map (\(w, shift) -> (fromIntegral w :: Int) `shiftL` shift) 

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral

--define a new symbol
chain2 :: Parse a -> Parse b -> (a -> b -> c) -> Parse c
chain2 parseA parseB func = Parse (\state -> case runParse parseA state of
                Left err -> Left err
                Right (x, newState) -> case runParse parseB newState of 
                        Left err -> Left err
                        Right (y, newState') -> Right (func x y, newState')
                )

--apply parseA for n times and got Parse [a]
chainN :: Parse a -> Int -> Parse [a]
chainN parseA n = case n of
        0 -> Parse (\state -> Right ([], state))
        i' ->  chain2 parseA (chainN parseA (i' - 1)) (\x y  -> x : y)
                    
parsePairString :: Parse (String, String)
parsePairString = chain2 parseString parseString (\x y -> (x, y))

parseStringMap :: Parse [(String, String)]
parseStringMap = parseInt >==? (\i -> chainN parsePairString i)

parseStringSet :: Parse [String]
parseStringSet = parseInt >==? (\i -> chainN parseString i)

parseString :: Parse String 
parseString = Parse parse'
        where parse' state = case runParse parseVInt state of
                Left err -> Left err
                Right (ii, newState) -> Right (LC.unpack $  L.take i $ string newState, ParseState $ L.drop i  $ string newState)
                        where i = toInt64 ii

parseBool :: Parse Bool
parseBool = Parse parse'
        where parse' state = if n == 1 then Right (True, ParseState rest)
                                else Right (False, ParseState rest)
                where n = toInt $ (!!0) $ unpack $ L.take 1 s
                      rest = L.drop 1 s
                      s = string state

parseVInt :: Parse Int
parseVInt = Parse $ Right . second ParseState . parse' 0 
        where parse' pos state = if n > 0  
                then (n, rest)
                else (((fst $ parse' (pos + 1) (ParseState rest)) `shiftL` 7) + n, rest)
                where n = toInt $ (!!0) $ unpack $ L.take 1 s
                      rest = L.drop 1 s
                      s = string state


--and then think about how to chain parse and ignore pattern match in the middle of the parsing
--try to parsing lucene segment header
--the segment value sequence is 
--magicNum -> CodecVersion -> currentVersion -> LuceneVersion -> docCount -> isCompoundFile
main = do h <- openFile "some.si" ReadMode
          contents <- L.hGetContents h
          let p = parseInt >== parseString >== parseInt >== parseString >== parseInt >== parseBool >== parseStringMap >== parseStringSet
          let ret1 = runParse  p $ ParseState contents
          print ret1
          --readInt f >>= print
          --codec <- readString f
          --print codec
          --actual_version <- readInt f
          --print actual_version
          --version <- readString f
          --print version
          --well done, got segment header
