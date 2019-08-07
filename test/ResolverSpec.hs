module ResolverSpec where

import qualified Data.Map as M
import           Test.Hspec hiding (Arg)
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


spec :: Spec
spec = describe "resolver" $ do
  it "should not crash with an impossible case" $ do
    res <- resolve testerResolver $ Tester M.empty $ M.singleton "bar" (ANil, ())
    res `shouldBe` Tester M.empty (M.singleton "bar" 5)

  it "should resolve arg fields" $ do
    res <- resolve testerResolver $ Tester (M.singleton "foo" (Arg "sandy":@@ ANil, ())) M.empty
    res `shouldBe` Tester (M.singleton "foo" "sandy") M.empty

  it "should resolve everything" $ do
    res <- resolve testerResolver
         $ Tester (M.singleton "foo" (Arg "sandy":@@ ANil, ()))
                  (M.singleton "bar" (ANil, ()))
    res `shouldBe` Tester (M.singleton "foo" "sandy")
                          (M.singleton "bar" 5)

