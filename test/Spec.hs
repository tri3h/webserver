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
  User.main
  Tag.main
  Author.main
  Image.main
  Category.main
  Comment.main
  Post.main
  Draft.main
