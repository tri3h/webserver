import qualified CategorySpec
import qualified DraftSpec
import qualified PostSpec
import Test.Hspec
import qualified UserSpec

main :: IO ()
main = do
  UserSpec.main
  CategorySpec.main
  PostSpec.main
  DraftSpec.main
