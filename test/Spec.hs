import Test.QuickCheck
import           Text.PrettyPrint.HughesPJ
import           Data.Attoparsec.ByteString.Char8
import Parser
import TestData
import Ppr
import qualified Data.ByteString.Char8 as BS8
import SchemaGenerator

main :: IO ()
main = do
  q <- generate arbitrary
  print $ pprQuery q
  print $ (parseOnly (queryParser @User) $ BS8.pack $ render $ pprQuery @User q) == Right q
