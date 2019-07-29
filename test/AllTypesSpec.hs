module AllTypesSpec where

import Data.List
import Test.Hspec hiding (Arg)
import Text.PrettyPrint.HughesPJ (render)
import Weft.Generics.AllTypes
import Weft.Types

newtype Id = Id String deriving (Generic, Show, Eq, Ord)
newtype Name = Name String deriving (Generic, Show, Eq, Ord)

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe String) -> Id)
  , userName       :: Magic ts Name
  , userBestFriend :: Magic ts (Arg "arg" (Maybe String) -> User ts)
  , userFriends    :: Magic ts [User ts]
  , userAccount    :: Magic ts (Account ts)
  , userFingers    :: Magic ts [Finger ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts) => Show (User ts)
deriving instance AllHave Eq (User ts)   => Eq (User ts)

data Account ts = Account
  { accountBalance :: Magic ts (Arg "num" (Maybe Int) -> Int)
  } deriving (Generic)

deriving instance AllHave Show (Account ts) => Show (Account ts)
deriving instance AllHave Eq (Account ts)   => Eq (Account ts)

data Finger ts = Finger
  { fingers :: Magic ts (Account ts)
  } deriving (Generic)

deriving instance AllHave Show (Finger ts) => Show (Finger ts)
deriving instance AllHave Eq (Finger ts)   => Eq (Finger ts)


main = hspec spec


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


