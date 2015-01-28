--failimar with System.directory and 
--System.Directory is a System-independent interface to directory manipulation

import System.Directory
import System.FilePath
import Data.List (intersperse)

systemDirectory= [ 
            getCurrentDirectory, 
            getHomeDirectory,
            getUserDocumentsDirectory,
            getTemporaryDirectory,
            doesFileExist "/Users/shuaizki/dev/haskell/refamiliar/real_world_haskell/chapter8/array.hs" >>= return . show,
            doesDirectoryExist "/Users/shuaizki/dev/haskell/refamiliar/real_world_haskell/chapter8" >>= return . show
           ]

systemFilePath = [
                        print pathSeparator,
                        print pathSeparators,
                        print searchPathSeparator,
                        print extSeparator,
                        getSearchPath >>= mapM_ print,
                        print (splitSearchPath "/shuaizki/tmp:/oaijodf/oaidjfo"),
                        --extension
                        print $ splitExtension "oajdof.exe",
                        print $ takeExtension "oajdof.doc",
                        print $ dropExtension "oajdof.doc",
                        --drive
                        print $ splitDrive "//oiajodfjoajd/oaidjof/oaijdfo.sh",
                        print $ splitDrive "/oiajodfjoajd/oaidjof/oaijdfo.sh",
                        --file path
                        print $ splitFileName "/oiajodfjoajd/oaidjof/oaijdfo.sh",
                        print $ takeFileName "/oiajodfjoajd/oaidjof/oaijdfo.sh",
                        print $ dropFileName "/oiajodfjoajd/oaidjof/oaijdfo.sh",
                        print $ combine "/oiajodfjoajd/oaidjof/tools" "operate.sh",
                        print $ splitPath "/oiajodfjoajd/oaidjof/tools",
                        --splitPath without trailing slashes
                        print $ splitDirectories "/oiajodfjoajd/oaidjof/tools"
                 ]
 
main = do sequence_ systemFilePath
