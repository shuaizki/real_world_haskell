--write some code to help thinking
import Data.ByteString.Lazy (ByteString(..), hGet, length, unpack, pack, hGetContents)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map.Lazy as LM
import qualified Data.Set as S
import Data.Functor
import Control.Monad
import Control.Applicative
import Data.Int
import System.IO
import Data.Bits
import Control.Arrow (second)
import Data.Word

--chain2 :: Parse a -> Parse b -> (a -> b -> c) -> Parse c
--chain2 parseA parseB func = Parse (\state -> case runParse parseA state of
--                Left err -> Left err
--                Right (x, newState) -> case runParse parseB newState of 
--                        Left err -> Left err
--                        Right (y, newState') -> Right (func x y, newState')
--                )

--parseStringSet :: Parse [String]
--parseStringSet = parseInt >==? (\i -> chainN parseString i)

--apply parseA for n times and got Parse [a]
--chainN :: Parse a -> Int -> Parse [a]
--chainN parseA n = case n of
--        0 -> Parse (\state -> Right ([], state))
--        i' ->  chain2 parseA (chainN parseA (i' - 1)) (\x y  -> x : y)
                    
--parsePairString :: Parse (String, String)
--parsePairString = chain2 parseString parseString (\x y -> (x, y))

--parseStringMap :: Parse [(String, String)]
--parseStringMap = parseInt >==? (\i -> chainN parsePairString i)


--parseString :: Parse String 
--parseString = Parse parse'
--        where parse' state = case runParse parseVInt state of
--                Left err -> Left err
--                Right (ii, newState) -> Right (LC.unpack $  L.take i $ string newState, ParseState $ L.drop i  $ string newState)
--                        where i = toInt64 ii


--define a function to store the current result into   
--parseInt :: Parse Int
--parseInt = Parse parse'
--        where parse' state = Right (sum $ processed $  zip (unpack bs) [24, 16, 8, 0], ParseState rest)
--                where bs = L.take 4 $ string state
--                      processed = map (\(w, shift) -> toInt w  `shiftL` shift)
--                      rest = L.drop 4 $ string state
--

--parseBool :: Parse Bool
--parseBool = Parse parse'
--        where parse' state = if n == 1 then Right (True, ParseState rest)
--                                else Right (False, ParseState rest)
--                where n = toInt $ (!!0) $ unpack $ L.take 1 s
--                      rest = L.drop 1 s
--                      s = string state

--parseVInt :: Parse Int
--parseVInt = Parse $ Right . second ParseState . parse' 0 
--        where parse' pos state = if n > 0  
--                then (n, rest)
--                else (((fst $ parse' (pos + 1) (ParseState rest)) `shiftL` 7) + n, rest)
--                      rest = L.drop 1 s
--                      s = string state


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

newtype Parse a = Parse {
                        runParse :: ParseState -> Either String (a, ParseState)
                  } 

instance Functor Parse where
        fmap f parseA = Parse (\state -> case runParse parseA state of
                                        Left err -> Left err
                                        Right (a, newState) -> Right (f a, newState)
                        )
instance Applicative Parse where
        pure a = Parse (\state -> Right (a, state))
        liftedFunc <*> parseA = Parse (\state -> case runParse parseA state of
                        Left err -> Left err
                        Right (a, newState) -> case runParse liftedFunc state of
                                Left err -> Left err
                                Right (func, newState') -> Right (func a, newState)
                )

--Parse a -> (a -> Parse b) -> Parse b
instance Monad Parse where 
        return = pure
        parseA >>= func = Parse (\state -> case runParse parseA state of
                        Left err -> Left err
                        Right (a, newState) -> (runParse $ func a) newState
                )
        parseA >> parseB = parseA >>= (\_ -> parseB)
        fail s = Parse (\_ -> Left s)

--monad >>=
(>==?) :: Parse a -> (a -> Parse b) -> Parse b
parseA >==? genParse = Parse chained 
        where chained state = case runParse parseA state of
                                Left err -> Left err
                                Right (a, state1) -> (runParse (genParse a)) state1 

-- monad >> ?
(>==) :: Parse a -> Parse b -> Parse b
parseA  >== parseB = Parse (\state -> 
                        case (runParse parseA) state of
                                Left err -> Left err
                                Right (a, state1) -> (runParse parseB) state1)

--need fmap
--pure ?
identity :: a -> Parse a
identity a = Parse (\x -> Right (a, x))

parse :: Parse a -> ParseState -> Either String a
parse parser state = case runParse parser state of
                        Left err -> Left err
                        Right (a, state) -> Right a

toInt :: (Integral a) => a -> Int
toInt = fromIntegral

readInt :: Handle -> IO Int
readInt h = do hGet h 4 >>= return . (\x -> zip (unpack x) [24, 16, 8, 0]) >>= return . sum . processed
                where processed = map (\(w, shift) -> (fromIntegral w :: Int) `shiftL` shift) 

toInt64 :: (Integral a) => a -> Int64
toInt64 = fromIntegral

--define a new symbol

chain2 :: Parse a -> Parse b -> (a -> b -> c) -> Parse c
chain2 parseA parseB func = parseA >>= \a -> (parseB >>= \b -> return (func a b))

parseStringMap :: Parse [(String, String)]
parseStringMap = parseInt >>= \n -> applyNTimes n (parseString >>= \s1 -> parseString >>= \s2 -> return (s1, s2))

parseStringSet :: Parse [String]
parseStringSet = parseInt >>= (\n -> applyNTimes n parseString)

parseString :: Parse String
parseString = parseVInt >>= \n -> (applyNTimes n parseByte) >>= return . (LC.unpack . L.pack)

applyNTimes :: Int -> Parse a -> Parse [a]
applyNTimes n parseA = foldl append (return[]) (replicate n parseA)

append :: Parse [a] -> Parse a -> Parse [a]
append b a = chain2 a b (\x y -> x:y)

getState :: Parse ParseState
getState = Parse (\state -> Right (state, state))

parseByte :: Parse Word8
parseByte = Parse parse'
        where parse' state = case L.length s of
                        0 -> Left "bytestring run out!!!"
                        _ -> Right (h, ParseState rest)
                        where h = head $ unpack s
                              rest = L.drop 1 s
                              s = string state

shiftLBy :: Int -> (Int -> Int)
shiftLBy i = (`shiftL` i) . toInt

chainList :: [Parse Int] -> (Int -> Int -> Int) -> Parse Int
chainList input func = foldl (\accParse newParse-> chain2 accParse newParse func) (return 0) input

parseInt :: Parse Int
parseInt = chainList parseList (+) 
        where parseList = map (\f -> fmap f (toInt <$> parseByte)) [shiftLBy 24, shiftLBy 16, shiftLBy 8, shiftLBy 0]

parseBool :: Parse Bool
parseBool = fmap (\i -> if i == 1 then True else False) $ fmap toInt parseByte

--[.*. 0x7f,]
shifts = [shiftLBy 0, shiftLBy 7, shiftLBy 14, shiftLBy 21, shiftLBy 28]
ands =  replicate 4 ((.&.) 0x7f) ++ [(.&.) 0x0f] :: [Int -> Int]

parseWhile :: (a -> Bool) -> Parse a -> Parse [a]
parseWhile f parseA = parseA >>= \a ->
                if not $ f a then (a:) <$> parseWhile f parseA
                       else return [a]

parseVInt :: Parse Int
parseVInt = (map toInt) <$> (parseWhile (\i -> i > 0) parseByte) >>= 
        return . sum . (zipWith3 (\x y e -> y (x e)) ands shifts)


--magicNum -> CodecVersion -> currentVersion -> LuceneVersion -> docCount -> isCompoundFile
main = do h <- openFile "some.si" ReadMode
          contents <- L.hGetContents h
          --let p = parseInt >== parseString >== parseInt >== parseString >== parseInt >== parseBool >== parseStringMap >== parseStringSet
          --print ret1
          let p = parseInt >>= \magic -> parseString >>= \codec
                -> parseInt >>= \version -> parseString >>= \lucene
                        -> parseInt >>= \docCount -> parseBool >>= \isCompound
                                -> parseStringMap >>= \m -> parseStringSet >>= \s
                                        -> return (SegmentInfo codec version lucene docCount isCompound m s)
          let ret1 = runParse  p $ ParseState contents
          print ret1

--well done~
