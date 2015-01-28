--implement a glob matching
--first translate a globbing expression to a regular expression
--then mathcing...
module GlobRegex (
        globToRegex,
        globToRegex',
        globMatched
       ) where

import qualified Data.ByteString.Lazy as L
import Text.Regex.Posix ((=~))
import System.Environment (getArgs)
import Data.Char (toLower)

--glob matching rules are
--1. ? match exectly one unknown charactor -> .?
--2. * match any number of charactors -> .*
--3. [chars] match a char which is member of chars -> [chars]
--4. [!chars] match any charactors but the chars set -> [^chars]
--5. [a-b] match a range -> [a-b]
--6. [!a-b] match any charactors but the range -> [^a-b]
--7. escape charactor

globMatched :: String -> String -> Bool -> Bool
globMatched source pattern case_sensitive = source' =~ (globToRegex pattern') :: Bool
                                where source' = if case_sensitive then source else (map toLower source)
                                      pattern' = if case_sensitive then pattern else (map toLower pattern)

globToRegex :: String -> String
globToRegex x = "^" ++ (globToRegex' x) ++ "$"

--some problems need to be improved
--1. didn't check if [ ends
globToRegex' :: String -> String
globToRegex' ('\\':x:xs) = '\\':x:globToRegex' xs
globToRegex' ('?':xs) = '.':'?': globToRegex' xs
globToRegex' ('*':xs) = '.':'*': globToRegex' xs
globToRegex' ('[':'!':xs) = '[':'^': globToRegex' xs
globToRegex' ('[':xs) = '[': close xs
globToRegex' (x:xs) = x : globToRegex' xs
globToRegex' [] = []

close :: String -> String
close (']':xs) = ']': globToRegex' xs
close (x:xs) = x: close xs
close [] = error "untermiuated ["
