import           Control.Monad.Reader
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8
import           Data.Either
import qualified Data.Map as M
import           GHC.Generics
import           Test.Hspec hiding (Arg)
import           Test.QuickCheck
import           TestData
import           Text.PrettyPrint.HughesPJ
import           Weft.Generics.PprQuery
import           Weft.Generics.QueryParser
import           Weft.Generics.Resolve
import           Weft.Internal.Types
import           Weft.Types

data Tester ts = Tester
  { testFoo :: Magic ts (Arg "name" String -> String)
  , testBar :: Magic ts Int
  } deriving Generic

deriving instance AllHave Show (Tester ts) => Show (Tester ts)
deriving instance AllHave Eq (Tester ts)   => Eq (Tester ts)


testerResolver :: Tester 'Resolver
testerResolver = Tester
  { testFoo = pure . getArg
  , testBar = pure 5
  }

testQuery
    :: ( Eq (record 'Query)
       , Wefty record
       )
    => record 'Query -> Bool
testQuery q
  = (== Right q)
  . parseOnly (flip runReaderT mempty queryParser)
  . BS8.pack
  . render
  $ pprQuery q


main :: IO ()
main = hspec $ do
  describe "resolver" $ do
    it "should not crash with an impossible case" $ do
      res <- resolve testerResolver $ Tester Nothing $ Just (ANil, ())
      res `shouldBe` Tester Nothing (Just 5)

    it "should resolve arg fields" $ do
      res <- resolve testerResolver $ Tester (Just (Arg "sandy":@@ ANil, ())) Nothing
      res `shouldBe` Tester (Just "sandy") Nothing

    it "should resolve everything" $ do
      res <- resolve testerResolver $ Tester (Just (Arg "sandy":@@ ANil, ()))
                                             (Just (ANil, ()))
      res `shouldBe` Tester (Just "sandy") (Just 5)


  describe "roundtrip parser" $ do
    it "should roundtrip for User" $
      property $ testQuery @User
    it "should roundtrip for Account" $
      property $ testQuery @Account

  describe "invalid arguments" $ do
    it "should fail if passed a fake argument" $ do
      parseOnly (flip runReaderT mempty $ queryParser @User)
                "{ userId(NOT_A_REAL_ARG: False) }"
        `shouldSatisfy` isLeft

  describe "variables" $ do
    it "should fail if referencing an unknown var" $ do
      parseOnly (flip runReaderT mempty $ queryParser @User)
                "{ userId(arg: $missing_var) }"
        `shouldSatisfy` isLeft
    it "should inline a known variable" $ do
      parseOnly (flip runReaderT (M.singleton "known" "\"a string\"")
                   $ queryParser @User)
                "{ userId(arg: $known) }"
        `shouldBe` Right ( User (Just ((Arg $ Just "a string") :@@ ANil, ()))
                                Nothing
                                Nothing
                                Nothing
                         )

