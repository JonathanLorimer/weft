import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8
import           Data.Either
import           Test.Hspec
import           Test.QuickCheck
import           TestData
import           Text.PrettyPrint.HughesPJ
import           Weft.Generics.PprQuery
import           Weft.Generics.QueryParser
import           Weft.Types

testQuery
    :: forall record
     . ( Eq (record 'Query)
       , Wefty record
       )
    => record 'Query -> Bool
testQuery q
  = (== Right q)
  . parseOnly (queryParser @record)
  . BS8.pack
  . render
  $ pprQuery @record q


main :: IO ()
main = hspec $ do
  describe "roundtrip parser" $ do
    it "should roundtrip for User" $
      property $ testQuery @User
    it "should roundtrip for Account" $
      property $ testQuery @Account

  describe "invalid arguments" $ do
    it "should fail if passed a fake argument" $ do
      parseOnly (queryParser @User) "{ userId(NOT_A_REAL_ARG: False) }"
        `shouldSatisfy` isLeft

