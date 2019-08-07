{-# LANGUAGE DeriveAnyClass             #-}

module ParserSpec where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bifunctor
import           Data.Either
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           Test.Hspec hiding (Arg)
import           Test.QuickCheck
import           Text.Megaparsec
import           Text.PrettyPrint.HughesPJ hiding (first)
import           Weft.Generics.PprQuery
import           Weft.Generics.QueryParser
import           Weft.Internal.Types
import           Weft.Types


type Parser = Parsec Void Text

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


-- | Like '==', but prints a counterexample when it fails.
infix 4 ====
(====) :: (Wefty record, Eq (record 'Query)) => Either String (record 'Query) -> record 'Query -> Property
Left x ==== _ = counterexample x False
Right x ==== y =
  flip counterexample res $ mconcat
    [ "\n\n --- got --- \n\n"
    , render $ pprQuery x
    , interpret res
    , render $ pprQuery y
    ]
  where
    res = x == y
    interpret True  = " == "
    interpret False = "\n\n --- but should be --- \n\n"

testQuery
    :: ( Eq (record 'Query)
       , Wefty record
       , Show (record 'Query)
       )
    => record 'Query -> Property
testQuery q
  = (==== q)
  . parseAllOnly (flip runReaderT mempty queryParser)
  . T.pack
  . render
  $ pprQuery q


parseAllOnly :: Parser a -> Text -> Either String a
parseAllOnly p = first errorBundlePretty . parse p "<test>"


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
        `shouldBe` Right ( User (M.singleton "userId" ((Arg $ Just "a string") :@@ ANil, ()))
                                M.empty
                                M.empty
                                M.empty
                         )

  describe "comments" $ do
    it "should allow comments everywhere yall" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @User) (T.pack $ unlines
        [ "userId #this is a comment"
        , "( # more"
        , "arg: #ok \"5\""
        , "\"6\" # dope"
        , ") # finished"
        ])
        `shouldBe` Right (User (M.singleton "userId" (Arg (Just "6") :@@ ANil, ()))
                               M.empty
                               M.empty
                               M.empty
                         )

    it "should not parse #s in strings" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @User) "userId(arg: \"# no problem\")"
        `shouldBe` Right (User (M.singleton "userId" (Arg (Just "# no problem") :@@ ANil, ()))
                               M.empty
                               M.empty
                               M.empty
                         )




