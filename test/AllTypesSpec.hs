module AllTypesSpec where

import Data.List
import Test.Hspec hiding (Arg)
import TestData
import Text.PrettyPrint.HughesPJ (render)
import Weft.Generics.AllTypes
import Weft.Types


spec :: Spec
spec = do
  describe "allTypes" $ do
    it "should find downstream types" $ do
      let all_types = sort $ fmap render $ allTypes @User

      length all_types `shouldBe` 3
      all_types !! 0 `shouldSatisfy` isPrefixOf "type Account"
      all_types !! 1 `shouldSatisfy` isPrefixOf "type Finger"
      all_types !! 2 `shouldSatisfy` isPrefixOf "type User"

    it "should not find upstream types" $ do
      let all_types = fmap render $ allTypes @Account

      length all_types `shouldBe` 1
      all_types !! 0 `shouldSatisfy` isPrefixOf "type Account"


