import Test.Hspec
import qualified AuthorSpec
import qualified UserSpec
import qualified CategorySpec


main :: IO ()
main = do 
    AuthorSpec.main
    UserSpec.main
    CategorySpec.main
