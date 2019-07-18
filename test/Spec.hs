import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8
import           GHC.Generics
import           Parser
import           Ppr
import           SchemaGenerator
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           TestData
import           Text.PrettyPrint.HughesPJ

main :: IO ()
main = do
  let test_query
          :: forall record prop
           . ( Eq (record 'Query)
             , Generic (record 'Query)
             , GEmptyQuery (Rep (record 'Query))
             , GIncrParser (Rep (record 'Query))
             , GPprQuery (Rep (record 'Query))
             )
          => record 'Query -> Bool
      test_query = \q -> (parseOnly (queryParser @record) $ BS8.pack $ render $ pprQuery @record q) == Right q

  quickCheck $ test_query @User
  quickCheck $ test_query @Account
