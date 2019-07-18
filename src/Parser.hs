module Parser where

import           Args
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.Char
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           SchemaGenerator
import           TestData


type HasEmptyQuery record =
  ( GEmptyQuery (Rep (record 'Query))
  , Generic (record 'Query)
  )

emptyQuery :: forall record . HasEmptyQuery record => record 'Query
emptyQuery = to $ gEmptyQuery @(Rep (record 'Query))

class GEmptyQuery (fq :: * -> *) where
    gEmptyQuery :: fq x

instance (GEmptyQuery fq, GEmptyQuery gq) => GEmptyQuery (fq :*: gq) where
    gEmptyQuery = (gEmptyQuery @fq) :*: (gEmptyQuery @gq)

instance GEmptyQuery fq => GEmptyQuery (M1 x y fq) where
    gEmptyQuery = M1 $ gEmptyQuery @fq

-- | Q3
instance GEmptyQuery (K1 x Bool) where
    gEmptyQuery = K1 False

-- | Q2
instance GEmptyQuery (K1 x (Maybe a)) where
    gEmptyQuery = K1 Nothing



testEmptyQuery :: User 'Query
testEmptyQuery = emptyQuery

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

queryParser :: (HasEmptyQuery record, HasIncrParser record) => Parser (record 'Query)
queryParser = do
  string $ "query"
  _ <- skipSpace
  _ <- char '{'
  _ <- skipSpace
  p <- incrParser emptyQuery
  _ <- skipSpace
  _ <- char '}'
  _ <- skipSpace
  pure p


instance ( GIncrParser rep fq
         , GIncrParser rep gq
         ) => GIncrParser rep (fq :*: gq) where
  gIncrParser get set rep = do
    rep' <-  gIncrParser (fstG . get) (\a b -> set (a :*: sndG (get b)) b) rep  <|> pure rep
    rep'' <- gIncrParser (sndG . get) (\a b -> set (fstG (get b) :*: a) b) rep' <|> pure rep'
    gIncrParser (fstG . get) (\a b -> set (a :*: sndG (get b)) b) rep'' <|> pure rep''


-- TODO(sandy): thiere is a bug here where if the args are necessary and you
-- dont put them in then the parser will succeed with the field being
-- un-asked-for
instance (KnownSymbol name, FromRawArgs args, IsAllMaybe args)
      => GIncrParser rep (M1 S ('MetaSel ('Just name) _1 _2 _3)
                         (K1 _4 (Maybe (Args args, ())))) where
  gIncrParser get set rep = do
    string $ BS.pack $ symbolVal $ Proxy @name
    skipSpace
    args <- parseOptionalArgs @args
    pure $ set (M1 $ K1 $ Just (args, ())) $ rep

instance ( KnownSymbol name
         , HasIncrParser t
         , HasEmptyQuery t
         , FromRawArgs args
         , IsAllMaybe args
         ) => GIncrParser rep (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                    (K1 _4 (Maybe (Args args, t 'Query)))) where
  gIncrParser get set rep = do
    string $ BS.pack $ symbolVal $ Proxy @name
    skipSpace
    args <- parseOptionalArgs @args
    skipSpace
    _ <- char '{'
    skipSpace
    z <- incrParser emptyQuery
    _ <- char '}'
    skipSpace
    pure $ set (M1 $ K1 $ Just (args, z)) $ rep


testIncrParser :: User 'Query -> Parser (User 'Query)
testIncrParser = incrParser

class IsAllMaybe (args :: [(Symbol, *)]) where
  isAllMaybe :: Maybe (Args args)

instance IsAllMaybe '[] where
  isAllMaybe = Just ANil

instance {-# OVERLAPPING #-} (KnownSymbol a, IsAllMaybe ts) => IsAllMaybe ('(a, Maybe b) ': ts) where
  isAllMaybe = (:@@) <$> (Just $ Arg Nothing) <*> isAllMaybe @ts

instance IsAllMaybe ('(a, b) ': ts) where
  isAllMaybe = Nothing


parseArgs :: FromRawArgs args => Parser (Args args)
parseArgs = do
  raw <- parseRawArgs
  maybe Control.Applicative.empty pure $ fromRawArgs raw


parseOptionalArgs :: forall args. (FromRawArgs args, IsAllMaybe args) => Parser (Args args)
parseOptionalArgs =
  case isAllMaybe @args of
    Nothing -> parseArgs
    Just argsOfNothing -> parseArgs <|> pure argsOfNothing



class HasParser ty where
  parseType :: Parser ty




parseARawArg :: Parser (String, String)
parseARawArg = do
  skipSpace
  first <- satisfy $ inClass "_A-Za-z"
  rest <- many $ satisfy $ inClass "_0-9A-Za-z"
  skipSpace
  _ <- char ':'
  skipSpace
  -- TODO(sandy): make this less shitty
  result <- many1 $ satisfy $ not . (Data.Char.isSpace)
  pure (first : rest, result)


parseRawArgs :: Parser (M.Map String String)
parseRawArgs = fmap M.fromList
             $ do
  skipSpace
  _ <- char '('
  skipSpace
  z <- parseARawArg `sepBy` (skipSpace >> char ',' >> skipSpace)
  skipSpace
  _ <- char ')'
  skipSpace
  pure z



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
    pure $ Arg readed :@@ args

instance {-# OVERLAPPING #-} (Read t, FromRawArgs args, KnownSymbol name) => FromRawArgs ('(name, Maybe t) ': args) where
  fromRawArgs raw = do
    args <- fromRawArgs @args raw
    asum
      [ do
         found <- M.lookup (symbolVal $ Proxy @name) raw
         readed <- listToMaybe $ fmap fst $ reads @t found
         pure $ Arg (Just readed) :@@ args
      , pure $ Arg Nothing :@@ args
      ]


