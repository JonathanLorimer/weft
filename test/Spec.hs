import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8
import           GHC.Generics
import           Weft.Generics.QueryParser
import           Weft.Generics.EmptyQuery
import           Ppr
import           Test.QuickCheck
import           TestData
import           Text.PrettyPrint.HughesPJ
import           Weft.Types

main :: IO ()
main = do
  let test_query
          :: forall record
           . ( Eq (record 'Query)
             , HasEmptyQuery (record)
             , HasQueryParser (record)
             , GPprQuery (Rep (record 'Query))
             )
          => record 'Query -> Bool
      test_query q = (== Right q)
                   . parseOnly (queryParser @record)
                   . BS8.pack
                   . render
                   $ pprQuery @record q

  quickCheck $ test_query @User
  quickCheck $ test_query @Account
