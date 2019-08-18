module ResolverSpec where

import Control.Monad
import Data.Void
import qualified Data.Map as M
import           Test.Hspec hiding (Arg)
import           Weft.Generics.Resolve
import           Weft.Internal.Types
import           Weft.Internal.Utils
import           Weft.Types


data Tester = Tester
  { testFoo :: Method '[ '("name", String) ] String
  , testBar :: Int
  } deriving Generic


testerResolver :: JHKD Tester 'Resolver
testerResolver =
  buildResolver @Tester
    (ToResolver $ pure . getArg)
    (ToResolver $ pure 5)


spec :: Spec
spec = describe "resolver" $ do
  it "should not crash with an impossible case" $ do
    res <- resolve testerResolver $
      buildQuery @Tester
        ( ToQuery $ M.empty )
        ( ToQuery $ M.singleton "bar" (ANil, ()) )
    res `shouldBe2`
      buildResponse @Tester
        ( ToResponse $ M.empty )
        ( ToResponse $ M.singleton "bar" 5 )

  it "should resolve arg fields" $ do
    res <- resolve testerResolver $
      buildQuery @Tester
        ( ToQuery $ M.singleton "foo" (Arg "sandy":@@ ANil, ()) )
        ( ToQuery $ M.empty )
    res `shouldBe2`
      buildResponse @Tester
        ( ToResponse $ M.singleton "foo" "sandy" )
        ( ToResponse $ M.empty )

  it "should resolve everything" $ do
    res <- resolve testerResolver $
      buildQuery @Tester
        ( ToQuery $ M.singleton "foo" (Arg "sandy":@@ ANil, ()) )
        ( ToQuery $ M.singleton "bar" (ANil, ()) )
    res `shouldBe2`
      buildResponse @Tester
        ( ToResponse $ M.singleton "foo" "sandy" )
        ( ToResponse $ M.singleton "bar" 5 )


shouldBe2
    :: ( HasCallStack
       , Eq (J record 'Response Void)
       )
    => JHKD record 'Response
    -> JHKD record 'Response
    -> Expectation
actual `shouldBe2` expected =
  expectTrue ("lame") $ runHKD actual == runHKD expected

expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

