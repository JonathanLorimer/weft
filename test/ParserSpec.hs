{-# LANGUAGE DeriveAnyClass             #-}

module ParserSpec where

import           BizzaroData
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Either
import qualified Data.List as L
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
import           Weft.Internal.Utils
import           Weft.Types


-- -- | Like ==, but prints a counterexample when it fails.
-- infix 4 ====
-- (====) :: (Wefty record, Eq (record 'Query)) => Either String (record 'Query) -> record 'Query -> Property
-- Left x ==== _ = counterexample x False
-- Right x ==== y =
--   flip counterexample res $ mconcat
--     [ "\n\n --- got --- \n\n"
--     , render $ pprQuery x
--     , interpret res
--     , render $ pprQuery y
--     ]
--   where
--     res = x == y
--     interpret True  = " == "
--     interpret False = "\n\n --- but should be --- \n\n"

testMagicQuery
    :: forall record
     . ( Eq (J record 'Query Void)
       , HasMagicQueryParser record
       , HasMagicPprQuery record
       , HasMagicRecordGen record 'Query
       )
    => Property
testMagicQuery = property $ do
  q <- magicRecordGen @record @'Query
  pure . (== Right q)
       . fmap runHKD
       . parseAllOnly (flip runReaderT mempty $ magicQueryParser @record)
       . T.pack
       . render
       $ magicPprQuery @record q


parseAllOnly :: Parser a -> Text -> Either String a
parseAllOnly p = first errorBundlePretty . parse p "<test>"


shouldSatisfy2 :: (HasCallStack) => a -> (a -> Bool) -> Expectation
v `shouldSatisfy2` p = expectTrue ("predicate failed on: ") (p v)

shouldBe2
    :: forall record
     . ( HasCallStack, Eq (J record 'Query Void)
       , HasMagicPprQuery record
       )
    => Either String (HKD record (ToMagic 'Query))
    -> HKD record (ToMagic 'Query)
    -> Expectation
actual `shouldBe2` expected =
  case fmap runHKD actual of
    Right e -> expectTrue (render $ magicPprQuery @record e) $ e == runHKD expected
    Left err -> expectTrue err False


expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)


spec :: Spec
spec = do
  describe "roundtrip parser" $ do
    it "should roundtrip for User" $
      property $ testMagicQuery @User
    it "should roundtrip for Account" $
      property $ testMagicQuery @Account

  describe "invalid arguments" $ do
    it "should fail if passed a fake argument" $ do
      parseAllOnly (flip runReaderT mempty $ magicQueryParser @User)
                " userId(NOT_A_REAL_ARG: False) "
        `shouldSatisfy2` isLeft

  describe "input types" $ do
    it "should parse an input type literal" $ do
      parseAllOnly (flip runReaderT mempty $ magicQueryParser @Finger)
                "fingers(input: {hearts: true, boots: 5}) { }"
        `shouldBe2`
          (
          buildQuery @Finger $ ToMagic $
                    M.singleton "fingers"
                      ( (Arg $ Just $ MyInputType 5 True) :@@ Arg Nothing :@@ ANil
                      , runHKD $ buildQuery @Account $ ToMagic M.empty
                      )
                )

  describe "enums types" $ do
    it "should parse an enum literal" $ do
      parseAllOnly (flip runReaderT mempty $ magicQueryParser @Finger)
                "fingers(enum: ONE) { }"
        `shouldBe2`
          (buildQuery @Finger $ ToMagic $
                    M.singleton "fingers"
                      ( Arg Nothing :@@ (Arg $ Just One) :@@ ANil
                      , runHKD $ buildQuery @Account $ ToMagic M.empty
                      ))

  describe "input types" $ do
    it "should parse an input type literal" $ do
      parseAllOnly (flip runReaderT mempty $ magicQueryParser @Finger)
                "fingers(input: {hearts: true, boots: 5}) { }"
        `shouldBe2`
         ( buildQuery @Finger $ ToMagic $
                    M.singleton "fingers"
                      ( (Arg $ Just $ MyInputType 5 True) :@@ Arg Nothing :@@ ANil
                      , runHKD $ buildQuery @Account $ ToMagic M.empty
                      )
                )

  describe "variables" $ do
    it "should fail if referencing an unknown var" $ do
      parseAllOnly (flip runReaderT mempty $ magicQueryParser @User) "userId(arg: $missing_var)"
        `shouldSatisfy2` isLeft

    it "should inline a known variable" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "1337")
                   $ magicQueryParser @User)
                "userId(arg: $known)"
        `shouldBe2` (
          buildQuery @User
            (ToMagic $  M.singleton "userId" ((Arg $ Just 1337) :@@ ANil, ()))
            (ToMagic M.empty)
            (ToMagic M.empty)
            (ToMagic M.empty)
            (ToMagic M.empty)
                         )

    it "should inline a known variable in an input type" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "1337")
                   $ magicQueryParser @Finger)
                "fingers(input: {hearts: true, boots: $known}) { }"
        `shouldBe2`
            ( buildQuery @Finger $ ToMagic $
                    M.singleton "fingers"
                      ( (Arg $ Just $ MyInputType 1337 True) :@@ Arg Nothing :@@ ANil
                      , runHKD $ buildQuery @Account $ ToMagic M.empty
                      )
                )

    it "should fail when var type is Int but receives String" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "\"1337\"")
                   $ magicQueryParser @User)
                "userId(arg: $known)"
        `shouldSatisfy2` \(Left s) -> L.isInfixOf "value that should have parsed as: Int" s

  describe "comments" $ do
    it "should allow comments everywhere yall" $ do
      parseAllOnly (flip runReaderT mempty $ magicQueryParser @User) (T.pack $ unlines
        [ "userId #this is a comment"
        , "( # more"
        , "arg: #ok 5"
        , "6 # dope"
        , ") # finished"
        ])
        `shouldBe2`
          (buildQuery @User (ToMagic $ M.singleton "userId" (Arg (Just 6) :@@ ANil, ()))
                            (ToMagic M.empty)
                            (ToMagic M.empty)
                            (ToMagic M.empty)
                            (ToMagic M.empty)
                         )

    it "should not parse #s in strings" $ do
      parseAllOnly (flip runReaderT mempty $ magicQueryParser @Account) "accountTitle(title: \"# no problem\")"
        `shouldBe2`
          (buildQuery @Account (ToMagic $ M.singleton "accountTitle" (Arg (Just "# no problem") :@@ ANil, ()))
                         )

  describe "directives" $ do
    it "should allow user to ignore fields using directives" $ do
      let userQ = buildQuery @User (ToMagic M.empty)
                                   (ToMagic $ M.singleton "userName" (ANil, ()))
                                   (ToMagic M.empty)
                                   (ToMagic M.empty)
                                   (ToMagic M.empty)
      parseAllOnly (flip runReaderT mempty $ magicQueryParser @User) (T.pack $ unlines
        [ "userId @include(if: false)"
        , "userName @include(if: true)"
        , "userBestFriend @skip(if: true) { userName }"
        , "userFriends @skip(if: false) { userName }"
        ])
        `shouldBe2`
          (buildQuery @User (ToMagic M.empty) -- userId
                            (ToMagic $ M.singleton "userName" (ANil, ()))
                            (ToMagic M.empty) -- bestFriend
                            (ToMagic $ M.singleton "userFriends" (ANil, runHKD userQ))
                            (ToMagic M.empty)
                         )

