{-# LANGUAGE DeriveAnyClass             #-}

module ParserSpec where

import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Either
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import           Test.Hspec hiding (Arg)
import           Test.QuickCheck
import           TestData
import           Text.Megaparsec
import           Text.PrettyPrint.HughesPJ hiding (first)
import           Weft.Generics.PprQuery
import           Weft.Generics.QueryParser
import           Weft.Internal.Types
import           Weft.Types


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

  describe "input types" $ do
    it "should parse an input type literal" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @Finger)
                "fingers(input: {hearts: true, boots: 5}) { }"
        `shouldBe`
          Right ( Finger $
                    M.singleton "fingers"
                      ( (Arg $ Just $ MyInputType 5 True) :@@ Arg Nothing :@@ ANil
                      , Account M.empty
                      )
                )

  describe "enums types" $ do
    it "should parse an enum literal" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @Finger)
                "fingers(enum: ONE) { }"
        `shouldBe`
          Right ( Finger $
                    M.singleton "fingers"
                      ( Arg Nothing :@@ (Arg $ Just One) :@@ ANil
                      , Account M.empty
                      )
                )

  describe "input types" $ do
    it "should parse an input type literal" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @Finger)
                "fingers(input: {hearts: true, boots: 5}) { }"
        `shouldBe`
          Right ( Finger $
                    M.singleton "fingers"
                      ( (Arg $ Just $ MyInputType 5 True) :@@ Arg Nothing :@@ ANil
                      , Account M.empty
                      )
                )

  describe "variables" $ do
    it "should fail if referencing an unknown var" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @User) "userId(arg: $missing_var)"
        `shouldSatisfy` isLeft

    it "should inline a known variable" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "1337")
                   $ queryParser @User)
                "userId(arg: $known)"
        `shouldBe` Right ( User (M.singleton "userId" ((Arg $ Just 1337) :@@ ANil, ()))
                                M.empty
                                M.empty
                                M.empty
                                M.empty
                         )

    it "should inline a known variable in an input type" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "1337")
                   $ queryParser @Finger)
                "fingers(input: {hearts: true, boots: $known}) { }"
        `shouldBe`
          Right ( Finger $
                    M.singleton "fingers"
                      ( (Arg $ Just $ MyInputType 1337 True) :@@ Arg Nothing :@@ ANil
                      , Account M.empty
                      )
                )

    it "should fail when var type is Int but receives String" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "\"1337\"")
                   $ queryParser @User)
                "userId(arg: $known)"
        `shouldSatisfy` \(Left s) -> L.isInfixOf "value that should have parsed as: Int" s

  describe "comments" $ do
    it "should allow comments everywhere yall" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @User) (T.pack $ unlines
        [ "userId #this is a comment"
        , "( # more"
        , "arg: #ok 5"
        , "6 # dope"
        , ") # finished"
        ])
        `shouldBe` Right (User (M.singleton "userId" (Arg (Just 6) :@@ ANil, ()))
                               M.empty
                               M.empty
                               M.empty
                               M.empty
                         )

    it "should not parse #s in strings" $ do
      parseAllOnly (flip runReaderT mempty $ queryParser @Account) "accountTitle(title: \"# no problem\")"
        `shouldBe` Right (Account (M.singleton "accountTitle" (Arg (Just "# no problem") :@@ ANil, ()))
                         )

  describe "directives" $ do
    it "should allow user to ignore fields using directives" $ do
      let userQ = User M.empty
                       (M.singleton "userName" (ANil, ()))
                       M.empty
                       M.empty
                       M.empty
      parseAllOnly (flip runReaderT mempty $ queryParser @User) (T.pack $ unlines
        [ "userId @include(if: false)"
        , "userName @include(if: true)"
        , "userBestFriend @skip(if: true) { userName }"
        , "userFriends @skip(if: false) { userName }"
        ])
        `shouldBe` Right (User M.empty -- userId
                               (M.singleton "userName" (ANil, ()))
                               M.empty -- bestFriend
                               (M.singleton "userFriends" (ANil, userQ))
                               M.empty
                         )

