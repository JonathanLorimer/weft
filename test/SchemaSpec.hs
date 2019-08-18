module SchemaSpec where

import Test.Hspec hiding (Arg)
import TestData
import Text.PrettyPrint.HughesPJ (render)
import Weft.Generics.PprSchema
import Weft.Generics.Schema

spec :: Spec
spec = do
  describe "ppr schema" $ do
    it "should ppr User" $ do
      render (pprSchema $ schema @User) `shouldBe` init (unlines
        [ "type User {"
        , "    id(arg: Int): ID!"
        , "    name: String!"
        , "    bestFriend(arg: Int): User!"
        , "    friends: [User!]"
        , "    fingers: [Finger!]"
        , "}"
        ])

