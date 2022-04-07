import Test.Hspec
import qualified AuthorSpec
import qualified UserSpec
import qualified CategorySpec
import qualified TagSpec
import qualified CommentSpec
import qualified PostSpec

main :: IO ()
main = do 
    AuthorSpec.main
    UserSpec.main
    CategorySpec.main
    TagSpec.main
    CommentSpec.main
    PostSpec.main
