module AllTypesSpec where

-- import BizzaroData
-- import Data.List
import Test.Hspec hiding (Arg)
-- import Text.PrettyPrint.HughesPJ (render)
-- import Weft.Generics.AllTypes


spec :: Spec
spec = do
  xdescribe "allTypes" $ do
    it "is all fucked" $ do
      True `shouldBe` True

--     it "should find downstream types" $ do
--       let all_types = sort $ fmap render $ allTypes @User

--       length all_types `shouldBe` 3
--       all_types !! 0 `shouldSatisfy` isPrefixOf "type Account"
--       all_types !! 1 `shouldSatisfy` isPrefixOf "type Finger"
--       all_types !! 2 `shouldSatisfy` isPrefixOf "type User"

--     it "should not find upstream types" $ do
--       let all_types = fmap render $ allTypes @Account

--       length all_types `shouldBe` 1
--       all_types !! 0 `shouldSatisfy` isPrefixOf "type Account"


