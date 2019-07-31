{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Weft.Generics.QueryParser
  ( HasQueryParser
  , queryParser
  , Vars
  ) where

import           Control.Applicative
import           Control.Applicative.Permutations
import           Control.Monad.Reader
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits hiding (ErrorMessage (..))
import           Weft.Generics.EmptyQuery
import           Weft.Internal.Types


type HasQueryParser record =
     ( Generic (record 'Query)
     , GQueryParser (Rep (record 'Query))
     )

queryParser :: HasQueryParser record => ReaderT Vars Parser (record 'Query)
queryParser = lift skipSpace *> fmap to gQueryParser <* lift skipSpace


type Vars = M.Map String String


------------------------------------------------------------------------------
-- |
class GPermFieldsParser (rq :: * -> *) where
  gPermFieldsParser :: [ReaderT Vars Parser (rq x)]

instance {-# OVERLAPPABLE #-} GPermFieldsParser fq => GPermFieldsParser (M1 a b fq) where
  gPermFieldsParser = fmap M1 <$> gPermFieldsParser

instance ( GPermFieldsParser fq
         , GPermFieldsParser gq
         , forall x. (Monoid (fq x), Monoid (gq x))
         ) => GPermFieldsParser (fq :*: gq) where
  gPermFieldsParser =
      (fmap (:*: mempty) <$> gPermFieldsParser @fq)
      ++ (fmap (mempty :*:) <$> gPermFieldsParser @gq)
      -- ]

instance (KnownSymbol name, ParseArgs args, IsAllMaybe args)
      => GPermFieldsParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                               (K1 _4 (M.Map Text (Args args, ())))) where
  gPermFieldsParser = pure . fmap (M1 . K1) $ do
    let name = symbolVal $ Proxy @name
    alias <- lift $ parseAlias name
    _ <- lift $ string $ BS.pack name
    lift skipSpace
    args <- parseOptionalArgs @args
    pure $ M.singleton alias (args, ())


parseAlias :: String -> Parser Text
parseAlias defname = fmap (T.pack . fromMaybe defname) $ optional $ do
  i <- parseAnIdentifier
  skipSpace
  _ <- char ':'
  skipSpace
  pure i

instance ( KnownSymbol name
         , HasQueryParser t
         , HasEmptyQuery t
         , ParseArgs args
         , IsAllMaybe args
         ) => GPermFieldsParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                    (K1 _4 (M.Map Text (Args args, t 'Query)))) where
  gPermFieldsParser = pure
                    . fmap (M1 . K1)
                    $ do
    let name = symbolVal $ Proxy @name
    alias <- lift $ parseAlias name
    _ <- lift $ string $ BS.pack name
    lift skipSpace
    args <- parseOptionalArgs @args
    lift skipSpace
    _ <- lift $ char '{'
    lift skipSpace
    z <- queryParser @t
    _ <- lift $ char '}'
    lift skipSpace
    pure $ M.singleton alias (args, z)



------------------------------------------------------------------------------
-- |
class GQueryParser (rq :: * -> *) where
  gQueryParser :: ReaderT Vars Parser (rq x)

instance {-# OVERLAPPABLE #-} GQueryParser fq
      => GQueryParser (M1 a b fq) where
  gQueryParser = M1 <$> gQueryParser

instance ( GPermFieldsParser (fq :*: gq)
         , forall x. (Monoid (fq x), Monoid (gq x))
         )
      => GQueryParser (fq :*: gq) where
  gQueryParser = foldManyOf gPermFieldsParser

instance ( GPermFieldsParser (M1 _1 _2 (K1 _3 f))
         , Monoid f
         )
      => GQueryParser (M1 _1 _2 (K1 _3 f)) where
  gQueryParser = foldManyOf gPermFieldsParser


------------------------------------------------------------------------------
-- |
parseOptionalArgs
    :: ( ParseArgs args
       , IsAllMaybe args
       )
    => ReaderT Vars Parser (Args args)
parseOptionalArgs =
  case isAllMaybe of
    Nothing -> parseArgList
    Just argsOfNothing ->
      fmap (fromMaybe argsOfNothing)
        $ optional parseArgList

parseArgList :: ParseArgs args => ReaderT Vars Parser (Args args)
parseArgList = do
  _ <- lift $ char '('
  lift skipSpace
  z <- intercalateEffect (lift $ skipSpace >> char ',' >> skipSpace) $ parseArgs
  lift skipSpace
  _ <- lift $ char ')'
  lift skipSpace
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
  , lift $ (char '"') >> (:) <$> pure '"' <*> parseStringValue
  , lift $ many1 $ satisfy $ \c -> all ($ c)
      [ not . Data.Char.isSpace
      , (/= ')')
      , (/= '$')
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


parseAnIdentifier :: Parser String
parseAnIdentifier = do
  first <- satisfy $ inClass "_A-Za-z"
  rest <- many $ satisfy $ inClass "_0-9A-Za-z"
  pure $ first : rest


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


------------------------------------------------------------------------------
-- |
foldManyOf :: (Foldable t, Alternative f, Monoid m) => t (f m) -> f m
foldManyOf = fmap fold . many . asum

