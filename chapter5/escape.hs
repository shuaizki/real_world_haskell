import Data.List (replicate)
import Numeric (showHex)
import Data.Bits ((.&.), (.|.), shiftR)

smallHex :: Int -> String
smallHex x = "\\u" ++ (replicate (4 - length h) '0') ++ h
                where h = showHex x ""

astral :: Int -> String
astral n = smallHex (a + 0xd800) ++ smallHex (b + 0xdc00)
        where a = (n `shiftR` 10) .&. 0x3ff
              b = n .&. 0x3ff
