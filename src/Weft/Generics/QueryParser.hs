module Weft.Generics.QueryParser
  ( HasQueryParser
  , queryParser
  ) where

import           Control.Applicative
import           Control.Applicative.Permutations
import           Control.Monad.Reader
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Weft.Generics.EmptyQuery
import           Weft.Internal.Types


type HasQueryParser record =
     ( Generic (record 'Query)
     , GQueryParser (Rep (record 'Query))
     )

incrParser :: HasQueryParser record => ReaderT Vars Parser (record 'Query)
incrParser = fmap to gQueryParser


type Vars = M.Map String String


------------------------------------------------------------------------------
-- |
class GPermFieldsParser (rq :: * -> *) where
  gPermFieldsParser :: Permutation (ReaderT Vars Parser) (rq x)

instance {-# OVERLAPPABLE #-} GPermFieldsParser fq => GPermFieldsParser (M1 a b fq) where
  gPermFieldsParser = M1 <$> gPermFieldsParser

instance ( GPermFieldsParser fq
         , GPermFieldsParser gq
         ) => GPermFieldsParser (fq :*: gq) where
  gPermFieldsParser = (:*:) <$> gPermFieldsParser
                            <*> gPermFieldsParser

instance (KnownSymbol name, ParseArgs args, IsAllMaybe args)
      => GPermFieldsParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                         (K1 _4 (Maybe (Args args, ())))) where
  gPermFieldsParser = fmap (M1 . K1) $ toPermutationWithDefault Nothing $ do
    _ <- lift $ string $ BS.pack $ symbolVal $ Proxy @name
    lift skipSpace
    args <- parseOptionalArgs @args
    pure $ Just (args, ())

instance ( KnownSymbol name
         , HasQueryParser t
         , HasEmptyQuery t
         , ParseArgs args
         , IsAllMaybe args
         ) => GPermFieldsParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                    (K1 _4 (Maybe (Args args, t 'Query)))) where
  gPermFieldsParser = fmap (M1 . K1) $ toPermutationWithDefault Nothing $ do
    _ <- lift $ string $ BS.pack $ symbolVal $ Proxy @name
    lift skipSpace
    args <- parseOptionalArgs @args
    lift skipSpace
    _ <- lift $ char '{'
    lift skipSpace
    z <- incrParser
    _ <- lift $ char '}'
    lift skipSpace
    pure $ Just (args, z)



------------------------------------------------------------------------------
-- |
class GQueryParser (rq :: * -> *) where
  gQueryParser :: ReaderT Vars Parser (rq x)

instance {-# OVERLAPPABLE #-} GQueryParser fq => GQueryParser (M1 a b fq) where
  gQueryParser = M1 <$> gQueryParser

instance ( GPermFieldsParser (fq :*: gq)) => GQueryParser (fq :*: gq) where
  gQueryParser = intercalateEffect (lift skipSpace) gPermFieldsParser

instance GPermFieldsParser (M1 _1 _2 (K1 _3 f)) => GQueryParser (M1 _1 _2 (K1 _3 f)) where
  gQueryParser = runPermutation gPermFieldsParser


------------------------------------------------------------------------------
-- |
parseOptionalArgs :: forall args. (ParseArgs args, IsAllMaybe args) => ReaderT Vars Parser (Args args)
parseOptionalArgs =
  case isAllMaybe @args of
    Nothing -> parseArgList
    Just argsOfNothing ->
      fmap (fromMaybe argsOfNothing)
        $ optional parseArgList

parseArgList :: ParseArgs args => ReaderT Vars Parser (Args args)
parseArgList = do
  _ <- lift $ char '('
  z <- intercalateEffect (lift $ skipSpace >> char ',' >> skipSpace) $ parseArgs
  _ <- lift $ char ')'
  pure z


parseAnArg :: Read a => String -> ReaderT Vars Parser a
parseAnArg arg_name = do
  lift skipSpace
  _ <- lift $ string $ BS.pack arg_name
  lift skipSpace
  _ <- lift $ char ':'
  lift skipSpace
  -- TODO(sandy): make this less shitty
  result <- parseRawArgValue
  pure $ read result


parseRawArgValue :: ReaderT Vars Parser String
parseRawArgValue = choice
  [do
      _ <- lift $ char '$'
      ident <- lift parseAnIdentifier
      vars <- ask
      case M.lookup ident vars of
        -- TODO(sandy): this shouldn't return a string, since we potentially
        -- know it's the right type inside of the vars list
        Just res -> pure res
        Nothing -> lift (empty <?> ("Undefined variable " ++ ident))
  , lift (char '"') >> (:) <$> pure '"' <*> parseStringValue
  , lift $ many1 $ satisfy $ \c -> all ($ c)
      [ not . Data.Char.isSpace
      , (/= ')')
      , (/= '$')
      ]
  ]

parseStringValue :: ReaderT Vars Parser String
parseStringValue = do
  c <- lift peekChar
  case c of
    Just '"' -> lift (char '"') >> pure "\""
    Just '\\' -> do
      c1 <- lift anyChar
      c2 <- lift anyChar
      (++) <$> pure (c1 : c2 : [])
           <*> parseStringValue
    Just _ -> (:) <$> lift anyChar
                  <*> parseStringValue
    Nothing -> empty


parseAnIdentifier :: Parser String
parseAnIdentifier = do
  first <- satisfy $ inClass "_A-Za-z"
  rest <- many $ satisfy $ inClass "_0-9A-Za-z"
  pure $ first : rest


------------------------------------------------------------------------------
-- |
queryParser :: (HasEmptyQuery record, HasQueryParser record) => ReaderT Vars Parser (record 'Query)
queryParser = do
  _ <- lift $ char '{'
  _ <- lift skipSpace
  p <- incrParser
  _ <- lift skipSpace
  _ <- lift $ char '}'
  _ <- lift skipSpace
  pure p


------------------------------------------------------------------------------
-- |
class ParseArgs (args :: [(Symbol, *)]) where
  parseArgs :: Permutation (ReaderT Vars Parser) (Args args)

instance ParseArgs '[] where
  parseArgs = pure ANil

instance {-# OVERLAPPING #-}
         ( Read t
         , ParseArgs args
         , KnownSymbol n
         ) => ParseArgs ('(n, Maybe t) ': args) where
  parseArgs =
    -- 'Maybe' arguments are allowed to be missing
    (:@@) <$> fmap Arg ( toPermutationWithDefault Nothing
                       . fmap Just
                       . parseAnArg
                       . symbolVal
                       $ Proxy @n
                       )
          <*> parseArgs

instance ( Read t
         , ParseArgs args
         , KnownSymbol n
         ) => ParseArgs ('(n, t) ': args) where
  parseArgs =
    (:@@) <$> fmap Arg ( toPermutation
                       . parseAnArg
                       . symbolVal
                       $ Proxy @n
                       )
          <*> parseArgs

