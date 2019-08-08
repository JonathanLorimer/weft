module Weft.Internal.ParserUtils where

import           Control.Applicative hiding (many, some)
import           Control.Applicative.Permutations
import           Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (skipLineComment)
import           Weft.Internal.Types



skipCrap :: Parser ()
skipCrap = do
  space
  _ <- optional $ skipLineComment "#"
  space


parens :: Char -> Char -> ReaderT Vars Parser a -> ReaderT Vars Parser a
parens l r p = do
  _ <- lift $ do
    skipCrap
    _ <- char l
    skipCrap
  res <- p
  _ <- lift $ do
    skipCrap
    _ <- char r
    skipCrap
  pure $ res


runPermutations :: Permutation (ReaderT Vars Parser) a -> ReaderT Vars Parser a
runPermutations = intercalateEffect (lift $ skipCrap >> char ',' >> skipCrap)


wrapLabel :: String -> ErrorItem Char
wrapLabel t = Label $ '"' NE.:| t ++ "\""


parseAnIdentifier :: Parser String
parseAnIdentifier = do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_'
  pure $ first : rest

