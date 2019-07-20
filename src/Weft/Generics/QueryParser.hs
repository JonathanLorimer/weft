module Weft.Generics.QueryParser
  ( RequestType(..)
  , HasQueryParser
  , parseQ
  , parseM
  , parseS
  , queryParser
  , parseServerRequest
  ) where

import           Control.Applicative
import           Control.Applicative.Permutations
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Weft.Generics.EmptyQuery
import           Weft.Internal.Types


type HasQueryParser record =
     ( Generic (record 'Query)
     , GIncrParser (Rep (record 'Query))
     )

incrParser :: HasQueryParser record => AP.Parser (record 'Query)
incrParser = fmap to gIncrParser


------------------------------------------------------------------------------
-- |
class GPermFieldsParser (rq :: * -> *) where
  gPermFieldsParser :: Permutation AP.Parser (rq x)

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
    _ <- AP.string $ BS.pack $ symbolVal $ Proxy @name
    AP.skipSpace
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
    _ <- AP.string $ BS.pack $ symbolVal $ Proxy @name
    AP.skipSpace
    args <- parseOptionalArgs @args
    AP.skipSpace
    _ <- AP.char '{'
    AP.skipSpace
    z <- incrParser
    _ <- AP.char '}'
    AP.skipSpace
    pure $ Just (args, z)



------------------------------------------------------------------------------
-- |
class GIncrParser (rq :: * -> *) where
  gIncrParser :: AP.Parser (rq x)

instance {-# OVERLAPPABLE #-} GIncrParser fq => GIncrParser (M1 a b fq) where
  gIncrParser = M1 <$> gIncrParser

instance ( GPermFieldsParser (fq :*: gq)) => GIncrParser (fq :*: gq) where
  gIncrParser = intercalateEffect AP.skipSpace gPermFieldsParser

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
parseOptionalArgs :: forall args. (FromRawArgs args, IsAllMaybe args) => AP.Parser (Args args)
parseOptionalArgs =
  case isAllMaybe @args of
    Nothing -> parseArgs
    Just argsOfNothing -> parseArgs <|> pure argsOfNothing


parseArgs :: FromRawArgs args => AP.Parser (Args args)
parseArgs = do
  raw <- parseRawArgs
  maybe Control.Applicative.empty pure $ fromRawArgs raw


parseRawArgs :: AP.Parser (M.Map String String)
parseRawArgs = fmap M.fromList $ do
  AP.skipSpace
  _ <- AP.char '('
  AP.skipSpace
  z <- parseARawArg `AP.sepBy` (AP.skipSpace >> AP.char ',' >> AP.skipSpace)
  AP.skipSpace
  _ <- AP.char ')'
  AP.skipSpace
  pure z


parseARawArg :: AP.Parser (String, String)
parseARawArg = do
  AP.skipSpace
  first <- AP.satisfy $ AP.inClass "_A-Za-z"
  rest <- many $ AP.satisfy $ AP.inClass "_0-9A-Za-z"
  AP.skipSpace
  _ <- AP.char ':'
  AP.skipSpace
  -- TODO(sandy): make this less shitty
  result <- parseRawArgValue
  pure (first : rest, result)


parseRawArgValue :: AP.Parser String
parseRawArgValue = AP.choice
  [ AP.char '"' >> (:) <$> pure '"' <*> parseStringValue
  , AP.many1 $ AP.satisfy $ \c -> all ($ c)
      [ not . Data.Char.isSpace
      , (/= ')')
      ]
  ]

parseStringValue :: AP.Parser String
parseStringValue = do
  c <- AP.peekChar
  case c of
    Just '"' -> AP.char '"' >> pure "\""
    Just '\\' -> do
      c1 <- AP.anyChar
      c2 <- AP.anyChar
      (++) <$> pure (c1 : c2 : [])
           <*> parseStringValue
    Just _ -> (:) <$> AP.anyChar
                  <*> parseStringValue
    Nothing -> empty


------------------------------------------------------------------------------
-- |

data RequestType s = QueryRequest s
                   | MutationRequest s
                   | SubscriptionRequest s
                   deriving (Eq, Ord, Show)
instance Functor RequestType where
    fmap f (QueryRequest x) = QueryRequest $ f x
    fmap f (MutationRequest x) = MutationRequest $ f x
    fmap f (SubscriptionRequest x) = SubscriptionRequest $ f x

parseRequestSignature :: BS.ByteString 
                      -> (BS.ByteString -> RequestType BS.ByteString)
                      -> AP.Parser (RequestType BS.ByteString)
parseRequestSignature str constructor = do
    body <- AP.string str
    _ <- AP.skipSpace
    pure $ constructor (body)

parseQ :: AP.Parser (RequestType BS.ByteString)
parseQ = parseRequestSignature "query" QueryRequest

parseM :: AP.Parser (RequestType BS.ByteString)
parseM = parseRequestSignature "mutation" MutationRequest

parseS :: AP.Parser (RequestType BS.ByteString)
parseS = parseRequestSignature "subscription" SubscriptionRequest

parseRequestType :: AP.Parser (RequestType BS.ByteString)
parseRequestType = do
      _ <- AP.skipSpace
      AP.choice [ parseQ
                , parseM
                , parseS
                ]

parseServerRequest :: BS.ByteString -> Either String (RequestType BS.ByteString)
parseServerRequest s = foldResult $ AP.parse parseRequestType s

foldResult :: AP.Result (RequestType BS.ByteString) -> Either String (RequestType BS.ByteString)
foldResult (AP.Done body constructor) = Right $ (const body) <$> constructor
foldResult (AP.Partial _) = Left "Partial"
foldResult (AP.Fail _ _ _) = Left "server request failed to parse"
------------------------------------------------------------------------------
-- |

queryParser :: (HasEmptyQuery record, HasQueryParser record) => AP.Parser (record 'Query)
queryParser = do
  _ <- AP.char '{'
  _ <- AP.skipSpace
  p <- incrParser
  _ <- AP.skipSpace
  _ <- AP.char '}'
  _ <- AP.skipSpace
  pure p

