import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8
import           Test.QuickCheck
import           TestData
import           Text.PrettyPrint.HughesPJ
import           Weft.Generics.PprQuery
import           Weft.Generics.QueryParser
import           Weft.Types


foo :: Gql GqlQuery a b 'Query
Right foo = parseOnly queryParser "{ query { getAllUsers { userId } } }"



testQuery
    :: forall record
     . ( Eq (record 'Query)
       , Wefty record
       )
    => record 'Query -> Bool
testQuery q
  = (== Right q)
  . parseOnly (queryParser @record)
  . BS8.pack
  . render
  $ pprQuery @record q


main :: IO ()
main = do
  quickCheck $ testQuery @User
  quickCheck $ testQuery @Account

