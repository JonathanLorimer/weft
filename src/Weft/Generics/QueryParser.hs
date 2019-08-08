{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Weft.Generics.QueryParser
  ( HasQueryParser
  , Vars
  , Parser
  , queryParser
  , anonymousQueryParser
  ) where

import           Control.Applicative hiding (many, some)
import           Control.Applicative.Permutations
import           Control.Monad.Reader
import           Data.Char
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Void
import           GHC.Generics
import qualified GHC.TypeLits as TL
import           GHC.TypeLits hiding (ErrorMessage (..))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (charLiteral, skipLineComment)
import           Weft.Generics.EmptyQuery
import           Weft.Internal.Types


identWithNum :: Parser Char
identWithNum = alphaNumChar <|> char '_'

identWithoutNum :: Parser Char
identWithoutNum = letterChar <|> char '_'

type Parser = Parsec Void Text


type HasQueryParser record =
     ( Generic (record 'Query)
     , GQueryParser (Rep (record 'Query))
     )

queryParser :: HasQueryParser record => ReaderT Vars Parser (record 'Query)
queryParser = lift skipCrap *> fmap to gQueryParser <* lift skipCrap


anonymousQueryParser :: HasQueryParser q => ReaderT Vars Parser (Gql q m s 'Query)
anonymousQueryParser = do
  r <- parens '{' '}' queryParser
  pure $ Gql $ M.singleton "query" (ANil, r)

type Vars = M.Map String String


skipCrap :: Parser ()
skipCrap = do
  space
  _ <- optional $ skipLineComment "#"
  space


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
  -- TODO(sandy): free perf gains here
  gPermFieldsParser =
      (fmap (:*: mempty) <$> gPermFieldsParser @fq)
      ++ (fmap (mempty :*:) <$> gPermFieldsParser @gq)

instance (KnownSymbol name, ParseArgs args, IsAllMaybe args)
      => GPermFieldsParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                               (K1 _4 (M.Map Text (Args args, ())))) where
  gPermFieldsParser = pure . fmap (M1 . K1) $ do
    let name = symbolVal $ Proxy @name
    alias <- lift $ try $ parseIdentOrAlias name
    lift skipCrap
    args <- parseOptionalArgs @args
    pure $ M.singleton alias (args, ())



parseIdentOrAlias :: String -> Parser Text
parseIdentOrAlias def = do
  asum
    [ try $ do
        _ <- string $ T.pack def
        skipCrap
        notFollowedBy (char ':')
        skipCrap
        pure $ T.pack def
    , do
        a <- parseAnIdentifier
        skipCrap
        _ <- char ':'
        skipCrap
        _ <- string $ T.pack def
        skipCrap
        pure $ T.pack a
    ]

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
    alias <- lift $ try $ parseIdentOrAlias name
    lift skipCrap
    args <- parseOptionalArgs @args
    z <- parens '{' '}' $ queryParser @t
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
parseArgList = parens '(' ')' $ runPermutations parseArgs


runPermutations :: Permutation (ReaderT Vars Parser) a -> ReaderT Vars Parser a
runPermutations = intercalateEffect (lift $ skipCrap >> char ',' >> skipCrap)


parseAnArg :: (Typeable a, ParseArgValue a) => String -> ReaderT Vars Parser a
parseAnArg arg_name = do
  lift skipCrap
  _ <- lift $ string $ T.pack arg_name
  lift skipCrap
  _ <- lift $ char ':'
  lift skipCrap
  -- TODO(sandy): make this less shitty
  result <- parseRawArgValue
  lift skipCrap
  pure result

wrapLabel :: String -> ErrorItem Char
wrapLabel t = Label $ '"' NE.:| t ++ "\""

parseRawArgValue :: forall a . (Typeable a, ParseArgValue a) => ReaderT Vars Parser a
parseRawArgValue = choice
  [do
      ident <- lift $ char '$' *> parseAnIdentifier
      vars <- ask
      case M.lookup ident vars of
        Just res ->
          case parseMaybe (runReaderT parseArgValue vars) (T.pack res) of
                      Just a -> pure a
                      Nothing -> failure Nothing
                              $ S.singleton
                              $ Label
                              $ NE.fromList
                              $ ("value that should have parsed as: " ++)
                              $ show $ typeRep $ Proxy @a
        Nothing -> lift $
          failure (Just $ wrapLabel ident) $ S.fromList $ fmap wrapLabel $ M.keys vars
  , parseArgValue <* lift skipCrap
  ]

parseStringValue :: Parser String
parseStringValue = char '"' >> manyTill charLiteral (char '"')


parseAnIdentifier :: Parser String
parseAnIdentifier = do
  first <- identWithoutNum
  rest <- many identWithNum
  pure $ first : rest


------------------------------------------------------------------------------
-- |
class ParseArgs (args :: [(Symbol, *)]) where
  parseArgs :: Permutation (ReaderT Vars Parser) (Args args)

instance ParseArgs '[] where
  parseArgs = pure ANil

instance {-# OVERLAPPING #-}
         ( ParseArgValue t
         , Typeable t
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

instance ( ParseArgValue t
         , Typeable t
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

class ParseArgValue a where
  parseArgValue :: ReaderT Vars Parser a

instance ParseArgValue String where
  parseArgValue = lift parseStringValue

instance ParseArgValue Bool where
  parseArgValue = lift $ asum
    [ False <$ string "false"
    , True  <$ string "true"
    ]

instance ParseArgValue Integer where
  parseArgValue = lift $ do
    neg <- optional $ char '-'
    num <- some digitChar
    pure $ read $ maybe "" pure neg ++ num

instance ParseArgValue Int where
  parseArgValue = fromInteger <$> parseArgValue

instance ParseArgValue Double where
  parseArgValue = lift $ do
    neg <- optional $ char '-'
    num <- some digitChar
    dec <- optional $ do
      _ <- char '.'
      ('.' :) <$> some digitChar
    pure $ read $ maybe "" pure neg ++ num ++ fromMaybe "" dec

instance ParseArgValue ID where
  parseArgValue = asum
    [ ID . show <$> parseArgValue @Integer
    , ID <$> parseArgValue @String
    ]

instance {-# OVERLAPPABLE #-} (Generic a, GParseThing (Rep a)) => ParseArgValue a where
  parseArgValue = fmap to gParseThing

------------------------------------------------------------------------------
-- |
foldManyOf
    :: (Functor t, Foldable t, MonadPlus f, MonadParsec e s f, Monoid m)
    => t (f m)
    -> f m
foldManyOf = fmap fold . many . asum

class GParseThing (g :: * -> *) where
  gParseThing :: ReaderT Vars Parser (g x)

instance  GParseThing f => GParseThing (M1 _1 _2 f) where
  gParseThing = M1 <$> gParseThing

instance GPermInputFields (f :*: g) => GParseThing (f :*: g) where
  gParseThing = do
    parens '{' '}' $ runPermutations gPermInputFields

instance GParseEnum (f :+: g) => GParseThing (f :+: g) where
  gParseThing = lift gParseEnum

class GParseEnum (g :: * -> *) where
  gParseEnum :: Parser (g x)

instance (GParseEnum f, GParseEnum g) => GParseEnum (f :+: g) where
  gParseEnum = L1 <$> gParseEnum <|> R1 <$> gParseEnum

instance (KnownSymbol name) => GParseEnum (M1 C ('MetaCons name _1 _2) U1) where
  gParseEnum  = M1 U1 <$ (string $ T.pack $ fmap toUpper $ symbolVal $ Proxy @name)

class GPermInputFields (g :: * -> *) where
  gPermInputFields :: Permutation (ReaderT Vars Parser) (g x)

instance ( GPermInputFields fq
         , GPermInputFields gq
         ) => GPermInputFields (fq :*: gq) where
  gPermInputFields = (:*:) <$> gPermInputFields
                           <*> gPermInputFields

instance (TypeError (TL.Text "shit is FUCKED yo"))
      => GPermInputFields (M1 S ('MetaSel 'Nothing _1 _2 _3) (K1 _4 a)) where
  gPermInputFields = undefined

instance (KnownSymbol name, ParseArgValue a, Typeable a)
      => GPermInputFields (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                (K1 _4 a)) where
  gPermInputFields = fmap (M1 . K1) $ toPermutation $ parseAnArg $ symbolVal $ Proxy @name

instance {-# OVERLAPPING #-} (KnownSymbol name, ParseArgValue a, Typeable a)
      => GPermInputFields (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                (K1 _4 (Maybe a))) where
  gPermInputFields = fmap (M1 . K1) $ toPermutationWithDefault Nothing $ fmap Just $ parseAnArg $ symbolVal $ Proxy @name

