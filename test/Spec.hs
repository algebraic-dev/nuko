import Syntax.Expr

import Test.Tasty.Golden (findByExtension)
import System.FilePath (dropExtension)
import Data.Foldable (for_)

main :: IO ()
main = do 
    filesNoExt <- map dropExtension 
         <$> findByExtension [".nk"] "test/suite"
    for_ filesNoExt print