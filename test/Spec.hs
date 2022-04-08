import qualified AuthorSpec
import qualified CategorySpec
import qualified CommentSpec
import qualified DraftSpec
import qualified PostSpec
import qualified TagSpec
import Test.Hspec
import qualified UserSpec

main :: IO ()
main = do
  AuthorSpec.main
  UserSpec.main
  CategorySpec.main
  TagSpec.main
  CommentSpec.main
  PostSpec.main
  DraftSpec.main
