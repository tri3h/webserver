import qualified Author
import qualified Category
import qualified Comment
import qualified Draft
import qualified Image
import qualified Post
import qualified Tag
import qualified User

main :: IO ()
main = do
  User.test
  Tag.test
  Author.test
  Image.test
  Category.test
  Comment.test
  Post.test
  Draft.test
