import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8
import           Test.QuickCheck
import           TestData
import           Text.PrettyPrint.HughesPJ
import           Weft.Generics.EmptyQuery
import           Weft.Generics.PprQuery
import           Weft.Generics.QueryParser
import           Weft.Types

main :: IO ()
main = do
  let test_query
          :: forall record
           . ( Eq (record 'Query)
             , HasEmptyQuery record
             , HasQueryParser record
             , HasPprQuery record
             )
          => record 'Query -> Bool
      test_query q = (== Right q)
                   . parseOnly (queryParser @record)
                   . BS8.pack
                   . render
                   $ pprQuery @record q

  quickCheck $ test_query @User
  quickCheck $ test_query @Account
