module ResolverSpec where

import           GHC.Generics
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
    res <- resolve testerResolver $ Tester Nothing $ Just (ANil, ())
    res `shouldBe` Tester Nothing (Just 5)

  it "should resolve arg fields" $ do
    res <- resolve testerResolver $ Tester (Just (Arg "sandy":@@ ANil, ())) Nothing
    res `shouldBe` Tester (Just "sandy") Nothing

  it "should resolve everything" $ do
    res <- resolve testerResolver $ Tester (Just (Arg "sandy":@@ ANil, ()))
                                           (Just (ANil, ()))
    res `shouldBe` Tester (Just "sandy") (Just 5)

