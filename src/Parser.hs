module Parser where

import           Args
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           QueryParser
import           SchemaGenerator
import           TestData


class GIncrParser rep fq where
  gIncrParser :: (rep x -> fq x) -> (fq x -> rep x -> rep x) -> rep x -> Parser (rep x)

instance {-# OVERLAPPABLE #-} GIncrParser rep fq => GIncrParser rep (M1 a b fq) where
  gIncrParser get set rep = gIncrParser (unM1 . get) (\a -> set (M1 a)) rep

fstG :: (f :*: g) x -> f x
fstG (f :*: _) = f

sndG :: (f :*: g) x -> g x
sndG (_ :*: g) = g

type HasIncrParser record =
     ( Generic (record 'Query)
     , GIncrParser (Rep (record 'Query))
                   (Rep (record 'Query))
     )

incrParser :: HasIncrParser record => record 'Query -> Parser (record 'Query)
incrParser = fmap to . gIncrParser id const . from

instance ( GIncrParser rep fq
         , GIncrParser rep gq
         ) => GIncrParser rep (fq :*: gq) where
  gIncrParser get set rep = do
    rep' <-  gIncrParser (fstG . get) (\a b -> set (a :*: sndG (get b)) b) rep  <|> pure rep
    rep'' <- gIncrParser (sndG . get) (\a b -> set (fstG (get b) :*: a) b) rep' <|> pure rep'
    gIncrParser (fstG . get) (\a b -> set (a :*: sndG (get b)) b) rep'' <|> pure rep''


instance KnownSymbol name
      => GIncrParser rep (M1 S ('MetaSel ('Just name) _1 _2 _3)
                         (K1 _4 (Maybe (Args args)))) where
  gIncrParser get set rep = do
    string $ T.pack $ symbolVal $ Proxy @name
    skipSpace
    pure $ set (M1 $ K1 undefined) $ rep

instance ( KnownSymbol name
         , HasIncrParser t
         , HasEmptyQuery t
         ) => GIncrParser rep (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                    (K1 _4 (Maybe (Args args, t 'Query)))) where
  gIncrParser get set rep = do
    string $ T.pack $ symbolVal $ Proxy @name
    skipSpace
    _ <- char '{'
    skipSpace
    z <- incrParser emptyQuery
    _ <- char '}'
    skipSpace
    pure $ set (M1 $ K1 $ Just (undefined, z)) $ rep


testIncrParser :: User' 'Query -> Parser (User' 'Query)
testIncrParser = incrParser

class IsAllMaybe (args :: [(Symbol, *)]) where
  isAllMaybe :: Bool

instance IsAllMaybe '[] where
  isAllMaybe = True

instance {-# OVERLAPPING #-} IsAllMaybe ts => IsAllMaybe ('(a, Maybe b) ': ts) where
  isAllMaybe = True && isAllMaybe @ts

instance IsAllMaybe ('(a, b) ': ts) where
  isAllMaybe = False



class HasParser ty where
  parseType :: Parser ty




parseARawArg :: Parser (String, String)
parseARawArg = do
  first <- satisfy $ inClass "_A-Za-z"
  rest <- many $ satisfy $ inClass "_0-9A-Za-z"
  skipSpace
  _ <- char ':'
  skipSpace
  -- TODO(sandy): make this less shitty
  result <- many1 $ satisfy $ not . isSpace
  pure (first : rest, result)

parseRawArgs :: Parser (M.Map String String)
parseRawArgs = fmap M.fromList
             $ parseARawArg `sepBy` (skipSpace >> char ',' >> skipSpace)



class FromRawArgs args where
  -- TODO(sandy): put in errors
  fromRawArgs :: M.Map String String -> Maybe (Args args)

instance FromRawArgs '[] where
  fromRawArgs = const $ pure ANil

instance (Read t, FromRawArgs args, KnownSymbol name) => FromRawArgs ('(name, t) ': args) where
  fromRawArgs raw = do
    args <- fromRawArgs @args raw
    found <- M.lookup (symbolVal $ Proxy @name) raw
    readed <- listToMaybe $ fmap fst $ reads @t found
    pure $ Label @name :> readed :@@ args






-- instance (ParseArgs args, KnownSymbol name, HasParser ty) => ParseArgs ('(name, ty) ': args) where
--   parseArgs = do
--     string $ T.pack $ symbolVal $ Proxy @name
--     skipSpace
--     _ <- char ':'
--     skipSpace
--     p <- parseType
--     pure $ _


