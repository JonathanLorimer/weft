{-# LANGUAGE DeriveAnyClass             #-}

module ParserSpec where

import           TestData
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


spec :: Spec
spec = do
  describe "roundtrip parser" $ do
    it "should roundtrip for User" $
      property $ testQuery @User
    it "should roundtrip for Account" $
      property $ testQuery @Account

  describe "invalid arguments" $ do
    it "should fail if passed a fake argument" $ do
      parseNoVars @User
            " id(NOT_A_REAL_ARG: False) "
        `shouldSatisfy2` isLeft

  describe "input types" $ do
    it "should parse an input type literal" $ do
      parseShouldBe @Finger "fingers(input: {hearts: true, boots: 5}) { }" $
        buildQuery @Finger $ ToQuery $
          M.singleton "fingers"
            ( (Arg $ Just $ MyInputType 5 True) :@@ Arg Nothing :@@ ANil
            , runHKD $ buildQuery @Account $ ToQuery M.empty
            )

  describe "enums types" $ do
    it "should parse an enum literal" $ do
      parseShouldBe @Finger "fingers(enum: ONE) { }" $
        buildQuery @Finger $ ToQuery $
          M.singleton "fingers"
            ( Arg Nothing :@@ (Arg $ Just One) :@@ ANil
            , runHKD $ buildQuery @Account $ ToQuery M.empty
            )

  describe "input types" $ do
    it "should parse an input type literal" $ do
      parseShouldBe @Finger "fingers(input: {hearts: true, boots: 5}) { }" $
        buildQuery @Finger $ ToQuery $
          M.singleton "fingers"
            ( (Arg $ Just $ MyInputType 5 True) :@@ Arg Nothing :@@ ANil
            , runHKD $ buildQuery @Account $ ToQuery M.empty
            )

  describe "variables" $ do
    it "should fail if referencing an unknown var" $ do
      parseNoVars @User "id(arg: $missing_var)"
        `shouldSatisfy2` isLeft

    it "should inline a known variable" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "1337") $ queryParser @User)
            "id(arg: $known)"
        `shouldBe2` (
          buildQuery @User
            (ToQuery $  M.singleton "id" ( (Arg $ Just 1337) :@@ ANil
                                         , ()
                                         ))
            (ToQuery M.empty)
            (ToQuery M.empty)
            (ToQuery M.empty)
            (ToQuery M.empty)
                    )

    it "should inline a known variable in an input type" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "1337") $ queryParser @Finger)
            "fingers(input: {hearts: true, boots: $known}) { }"
        `shouldBe2` (
          buildQuery @Finger $ ToQuery $
            M.singleton "fingers"
              ( (Arg $ Just $ MyInputType 1337 True) :@@ Arg Nothing :@@ ANil
              , runHKD $ buildQuery @Account $ ToQuery M.empty
              ))

    it "should fail when var type is Int but receives String" $ do
      parseAllOnly (flip runReaderT (M.singleton "known" "\"1337\"") $ queryParser @User)
            "id(arg: $known)"
        `shouldSatisfy2` \(Left s) ->
            L.isInfixOf "value that should have parsed as: Int" s

  describe "comments" $ do
    it "should allow comments everywhere yall" $ do
      parseShouldBe @User
        ( T.unlines
            [ "id #this is a comment"
            , "( # more"
            , "arg: #ok 5"
            , "6 # dope"
            , ") # finished"
            ]
        ) $ buildQuery @User
              (ToQuery $ M.singleton "id" (Arg (Just 6) :@@ ANil, ()))
              (ToQuery M.empty)
              (ToQuery M.empty)
              (ToQuery M.empty)
              (ToQuery M.empty)

    it "should not parse #s in strings" $ do
      parseShouldBe @Account "title(title: \"# no problem\")" $
        buildQuery @Account $ ToQuery $
          M.singleton "title"
            ( Arg (Just "# no problem") :@@ ANil
            , ()
            )

  describe "directives" $ do
    it "should allow user to ignore fields using directives" $ do
      let userQ =
            buildQuery @User
              (ToQuery M.empty)
              (ToQuery $ M.singleton "name" (ANil, ()))
              (ToQuery M.empty)
              (ToQuery M.empty)
              (ToQuery M.empty)
      parseShouldBe @User
        ( T.unlines
            [ "id @include(if: false)"
            , "name @include(if: true)"
            , "bestFriend @skip(if: true) { name }"
            , "friends @skip(if: false) { name }"
            ]
        ) $ buildQuery @User
              (ToQuery M.empty) -- userId
              (ToQuery $ M.singleton "name" (ANil, ()))
              (ToQuery M.empty) -- bestFriend
              (ToQuery $ M.singleton "friends" (ANil, runHKD userQ))
              (ToQuery M.empty)


testQuery
    :: forall record
     . ( Eq (J record 'Query Void)
       , HasQueryParser record
       , HasPprQuery record
       , HasRecordGen record 'Query
       )
    => Property
testQuery = property $ do
  q <- recordGen @record @'Query
  pure . (== Right (runHKD q))
       . fmap runHKD
       . parseNoVars @record
       . T.pack
       . render
       $ pprQuery q


parseAllOnly :: Parser a -> Text -> Either String a
parseAllOnly p = first errorBundlePretty . parse p "<test>"


parseNoVars
    :: forall record
     . HasQueryParser record
    => Text
    -> Either String (HKD record (ToMagic 'Query))
parseNoVars = parseAllOnly (flip runReaderT mempty $ queryParser @record)


shouldSatisfy2 :: (HasCallStack) => a -> (a -> Bool) -> Expectation
v `shouldSatisfy2` p = expectTrue "predicate failed" $ p v


shouldBe2
    :: ( HasCallStack
       , Eq (J record 'Query Void)
       , HasPprQuery record
       )
    => Either String (HKD record (ToMagic 'Query))
    -> HKD record (ToMagic 'Query)
    -> Expectation
actual `shouldBe2` expected =
  case actual of
    Right e -> expectTrue (render $ pprQuery e) $ runHKD e == runHKD expected
    Left err -> expectTrue err False


parseShouldBe
    :: ( HasCallStack
       , Eq (J record 'Query Void)
       , HasPprQuery record
       , HasQueryParser record
       )
    => Text
    -> HKD record (ToMagic 'Query)
    -> Expectation
parseShouldBe str expected = shouldBe2 (parseNoVars str) expected


expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

