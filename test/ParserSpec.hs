{-# LANGUAGE DeriveAnyClass             #-}

module ParserSpec where

import           Control.Monad.Reader
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8
import           Data.Either
import qualified Data.Map as M
import           Data.Aeson
import           Test.Hspec hiding (Arg)
import           Test.QuickCheck
import           Text.PrettyPrint.HughesPJ
import           Weft.Generics.PprQuery
import           Weft.Generics.QueryParser
import           Weft.Internal.Types
import           Weft.Types

data User ts = User
  { userId         :: Magic ts (Arg "arg" (Maybe String) -> Int)
  , userName       :: Magic ts String
  , userBestFriend :: Magic ts (Arg "arg" (Maybe String) -> User ts)
  , userFriends    :: Magic ts [User ts]
  } deriving (Generic)

deriving instance AllHave Show (User ts)     => Show (User ts)
deriving instance AllHave Eq (User ts)       => Eq (User ts)
deriving instance AllHave ToJSON (User ts)   => ToJSON (User ts)

data Account ts = Account
  { accountBalance :: Magic ts (Arg "num" (Maybe Int) -> Int)
  } deriving (Generic)

deriving instance AllHave Show (Account ts)     => Show (Account ts)
deriving instance AllHave Eq (Account ts)       => Eq (Account ts)
deriving instance AllHave ToJSON (Account ts)   => ToJSON (Account ts)

instance Arbitrary (Account 'Query) where
  arbitrary = recordGen

instance Arbitrary (User 'Query) where
  arbitrary = recordGen

instance Arbitrary (Account 'Data) where
  arbitrary = recordGen

instance Arbitrary (User 'Data) where
  arbitrary = recordGen

testQuery
    :: ( Eq (record 'Query)
       , Wefty record
       )
    => record 'Query -> Bool
testQuery q
  = (== Right q)
  . parseAllOnly (flip runReaderT mempty queryParser)
  . BS8.pack
  . render
  $ pprQuery q


parseAllOnly :: Parser a -> BS8.ByteString -> Either String a
parseAllOnly p = parseOnly (p <* endOfInput)



spec :: Spec
spec = do
  describe "roundtrip parser" $ do
    it "should roundtrip for User" $
      property $ testQuery @User
    it "should roundtrip for Account" $
      property $ testQuery @Account

  describe "invalid arguments" $ do
    it "should fail if passed a fake argument" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @User)
                " userId(NOT_A_REAL_ARG: False) "
        `shouldSatisfy` isLeft

  describe "variables" $ do
    it "should fail if referencing an unknown var" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @User) "userId(arg: $missing_var)"
        `shouldSatisfy` isLeft
    it "should inline a known variable" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "\"a string\"")
                   $ queryParser @User)
                "userId(arg: $known)"
        `shouldBe` Right ( User (Just ((Arg $ Just "a string") :@@ ANil, ()))
                                Nothing
                                Nothing
                                Nothing
                         )

