import Text.Regex.Posix
import Text.Regex.Base.RegexLike
import Data.Array
import GlobRegex (globToRegex, globToRegex')

source = " fofo bar fofo bar"
regex = "(fo|blah|fofo)"

--try all kinds of MatchResult
bool_result = source =~ regex :: Bool
b_result = source =~ regex :: String
tri_result = source =~ regex :: (String, String, String)
interval_result = source =~ regex :: (Int, Int)
mr  = source =~ regex :: (MatchResult String)
mr_before = mrBefore mr
mr_after = mrAfter mr
mr_match = mrMatch mr
mr_sublist = mrSubList mr
mr_subs = mrSubs mr
--match text
mr_mt = source =~ regex :: (String, MatchText String, String)
mr_quad = source =~ regex :: (String, String, String, [String])
mr_array = source =~ regex :: MatchArray
mr_all = getAllSubmatches ( source =~ regex :: AllSubmatches [] (Int, Int))
mr_text = getAllTextMatches (source =~ regex :: AllTextMatches [] (Array Int String))
mr_matches = getAllMatches (source =~ regex :: AllMatches [] (Int, Int))

m = source =~~ regex :: [String]

--test globToReg
myMatch :: String -> String -> (String, String, String)
myMatch = (=~)

source1  = "what the fuck [word] is short of WTF, f*k"
regex1 = ".?hat"
regex2 = ".*fuck"
regex3 = "[hos]rt"
regex4 = "[^a ]t"
regex5 = "[a-z]t"
regex6 = "[^a-z]t"
regex7 = "\\*k"
regex_list = [regex1, regex2, regex3, regex4, regex5, regex6, regex7]

glob_test = mapM_ print (map (myMatch source1) [regex1, regex2, regex3, regex4, regex5, regex6, regex7])

--test Translate done
glob_list = ["?hat", "*fuck", "[hos]rt", "[!a ]t", "[a-z]t", "[!a-z]t", "\\*k"]
--should add more complex test
glob_list2 = ["[foa]uck \\[*\\]", "[aoijdof"]

regex_list1 = map globToRegex' glob_list
regex_list2 = map globToRegex' glob_list2

glob_test2 = mapM_ print (map (myMatch source1) regex_list2)

main = do print (regex_list == regex_list1)
          print regex_list2
          glob_test2
