module Weft.Generics.QueryParser
  ( HasQueryParser
  , queryParser
  ) where

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
import           Weft.Generics.EmptyQuery
import           Weft.Types


type HasQueryParser record =
     ( Generic (record 'Query)
     , GIncrParser (Rep (record 'Query))
     )

incrParser :: HasQueryParser record => Parser (record 'Query)
incrParser = fmap to gIncrParser


------------------------------------------------------------------------------
-- |
class GPermFieldsParser (rq :: * -> *) where
  gPermFieldsParser :: Permutation Parser (rq x)

instance {-# OVERLAPPABLE #-} GPermFieldsParser fq => GPermFieldsParser (M1 a b fq) where
  gPermFieldsParser = M1 <$> gPermFieldsParser

instance ( GPermFieldsParser fq
         , GPermFieldsParser gq
         ) => GPermFieldsParser (fq :*: gq) where
  gPermFieldsParser = (:*:) <$> gPermFieldsParser
                          <*> gPermFieldsParser

-- TODO(sandy): thiere is a bug here where if the args are necessary and you
-- dont put them in then the parser will succeed with the field being
-- un-asked-for
instance (KnownSymbol name, FromRawArgs args, IsAllMaybe args)
      => GPermFieldsParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                         (K1 _4 (Maybe (Args args, ())))) where
  gPermFieldsParser = fmap (M1 . K1) $ toPermutationWithDefault Nothing $ do
    _ <- string $ BS.pack $ symbolVal $ Proxy @name
    skipSpace
    args <- parseOptionalArgs @args
    pure $ Just (args, ())

instance ( KnownSymbol name
         , HasQueryParser t
         , HasEmptyQuery t
         , FromRawArgs args
         , IsAllMaybe args
         ) => GPermFieldsParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                    (K1 _4 (Maybe (Args args, t 'Query)))) where
  gPermFieldsParser = fmap (M1 . K1) $ toPermutationWithDefault Nothing $ do
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



------------------------------------------------------------------------------
-- |
class GIncrParser (rq :: * -> *) where
  gIncrParser :: Parser (rq x)

instance {-# OVERLAPPABLE #-} GIncrParser fq => GIncrParser (M1 a b fq) where
  gIncrParser = M1 <$> gIncrParser

instance ( GPermFieldsParser (fq :*: gq)) => GIncrParser (fq :*: gq) where
  gIncrParser = intercalateEffect skipSpace gPermFieldsParser

instance GPermFieldsParser (M1 _1 _2 (K1 _3 f)) => GIncrParser (M1 _1 _2 (K1 _3 f)) where
  gIncrParser = runPermutation gPermFieldsParser


------------------------------------------------------------------------------
-- |
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



------------------------------------------------------------------------------
-- |
parseOptionalArgs :: forall args. (FromRawArgs args, IsAllMaybe args) => Parser (Args args)
parseOptionalArgs =
  case isAllMaybe @args of
    Nothing -> parseArgs
    Just argsOfNothing -> parseArgs <|> pure argsOfNothing


parseArgs :: FromRawArgs args => Parser (Args args)
parseArgs = do
  raw <- parseRawArgs
  maybe Control.Applicative.empty pure $ fromRawArgs raw


parseRawArgs :: Parser (M.Map String String)
parseRawArgs = fmap M.fromList $ do
  skipSpace
  _ <- char '('
  skipSpace
  z <- parseARawArg `sepBy` (skipSpace >> char ',' >> skipSpace)
  skipSpace
  _ <- char ')'
  skipSpace
  pure z


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


parseRawArgValue :: Parser String
parseRawArgValue = choice
  [ char '"' >> (:) <$> pure '"' <*> parseStringValue
  , many1 $ satisfy $ \c -> all ($ c)
      [ not . Data.Char.isSpace
      , (/= ')')
      ]
  ]

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


------------------------------------------------------------------------------
-- |
queryParser :: (HasEmptyQuery record, HasQueryParser record) => Parser (record 'Query)
queryParser = do
  _ <- char '{'
  _ <- skipSpace
  p <- incrParser
  _ <- skipSpace
  _ <- char '}'
  _ <- skipSpace
  pure p

