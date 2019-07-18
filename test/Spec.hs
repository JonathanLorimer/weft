import Test.QuickCheck
import           Text.PrettyPrint.HughesPJ
import           Data.Attoparsec.ByteString.Char8
import Parser
import TestData
import Ppr
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = quickCheck $ \q -> (parseOnly (queryParser @Account) $ BS8.pack $ render $ pprQuery @Account q) == Right q
