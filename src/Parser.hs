module Parser where

import           Control.Applicative
import           Control.Applicative.Permutations
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           TestData
import           Weft.Types


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

class GIncrPermParser (rq :: * -> *) where
  gIncrPermParser :: Permutation Parser (rq x)

instance {-# OVERLAPPABLE #-} GIncrPermParser fq => GIncrPermParser (M1 a b fq) where
  gIncrPermParser = M1 <$> gIncrPermParser

instance ( GIncrPermParser fq
         , GIncrPermParser gq
         ) => GIncrPermParser (fq :*: gq) where
  gIncrPermParser = (:*:) <$> gIncrPermParser
                          <*> gIncrPermParser

class GIncrParser (rq :: * -> *) where
  gIncrParser :: Parser (rq x)

instance {-# OVERLAPPABLE #-} GIncrParser fq => GIncrParser (M1 a b fq) where
  gIncrParser = M1 <$> gIncrParser

fstG :: (f :*: g) x -> f x
fstG (f :*: _) = f

sndG :: (f :*: g) x -> g x
sndG (_ :*: g) = g

type HasIncrParser record =
     ( Generic (record 'Query)
     , GIncrParser (Rep (record 'Query))
     )

incrParser :: HasIncrParser record => Parser (record 'Query)
incrParser = fmap to gIncrParser

queryParser :: (HasEmptyQuery record, HasIncrParser record) => Parser (record 'Query)
queryParser = do
  _ <- char '{'
  _ <- skipSpace
  p <- incrParser
  _ <- skipSpace
  _ <- char '}'
  _ <- skipSpace
  pure p


instance ( GIncrPermParser (fq :*: gq)) => GIncrParser (fq :*: gq) where
  gIncrParser = intercalateEffect skipSpace gIncrPermParser

instance GIncrPermParser (M1 _1 _2 (K1 _3 f)) => GIncrParser (M1 _1 _2 (K1 _3 f)) where
  gIncrParser = runPermutation gIncrPermParser


-- TODO(sandy): thiere is a bug here where if the args are necessary and you
-- dont put them in then the parser will succeed with the field being
-- un-asked-for
instance (KnownSymbol name, FromRawArgs args, IsAllMaybe args)
      => GIncrPermParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                         (K1 _4 (Maybe (Args args, ())))) where
  gIncrPermParser = fmap (M1 . K1) $ toPermutationWithDefault Nothing $ do
    _ <- string $ BS.pack $ symbolVal $ Proxy @name
    skipSpace
    args <- parseOptionalArgs @args
    pure $ Just (args, ())

instance ( KnownSymbol name
         , HasIncrParser t
         , HasEmptyQuery t
         , FromRawArgs args
         , IsAllMaybe args
         ) => GIncrPermParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                    (K1 _4 (Maybe (Args args, t 'Query)))) where
  gIncrPermParser = fmap (M1 . K1) $ toPermutationWithDefault Nothing $ do
    _ <- string $ BS.pack $ symbolVal $ Proxy @name
    skipSpace
    args <- parseOptionalArgs @args
    skipSpace
    _ <- char '{'
    skipSpace
    z <- incrParser
    _ <- char '}'
    skipSpace
    pure $ Just (args, z)


testIncrParser :: Parser (Account 'Query)
testIncrParser = incrParser


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

parseStringValue :: Parser String
parseStringValue = do
  c <- peekChar
  case c of
    Just '"' -> char '"' >> pure "\""
    Just '\\' -> do
      c1 <- anyChar
      c2 <- anyChar
      (++) <$> pure (c1 : c2 : [])
           <*> parseStringValue
    Just _ -> (:) <$> anyChar
                  <*> parseStringValue
    Nothing -> empty

parseRawArgValue :: Parser String
parseRawArgValue = choice
  [ char '"' >> (:) <$> pure '"' <*> parseStringValue
  , many1 $ satisfy $ \c -> all ($ c)
      [ not . Data.Char.isSpace
      , (/= ')')
      ]
  ]



parseARawArg :: Parser (String, String)
parseARawArg = do
  skipSpace
  first <- satisfy $ inClass "_A-Za-z"
  rest <- many $ satisfy $ inClass "_0-9A-Za-z"
  skipSpace
  _ <- char ':'
  skipSpace
  -- TODO(sandy): make this less shitty
  result <- parseRawArgValue
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


