{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Weft.Generics.QueryParser
  ( HasQueryParser
  , HasMagicQueryParser
  , Vars
  , Parser
  , queryParser
  , magicQueryParser
  , anonymousQueryParser
  ) where

import           Control.Applicative hiding (many, some)
import           Control.Applicative.Permutations
import           Control.Monad.Reader
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Void
import           GHC.Generics
import           GHC.TypeLits hiding (ErrorMessage (..))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Weft.Internal.ArgTypes
import           Weft.Internal.ParserUtils
import           Weft.Internal.Types



type HasQueryParser record =
     ( Generic (record 'Query)
     , GQueryParser (Rep (record 'Query))
     )

type HasMagicQueryParser record =
     ( Generic record
     , GQueryParser (J record 'Query)
     )

queryParser :: HasQueryParser record => ReaderT Vars Parser (record 'Query)
queryParser = lift skipCrap *> fmap to gQueryParser <* lift skipCrap

magicQueryParser :: HasMagicQueryParser record => ReaderT Vars Parser (J record 'Query Void)
magicQueryParser = lift skipCrap *> gQueryParser <* lift skipCrap


anonymousQueryParser :: HasQueryParser q => ReaderT Vars Parser (Gql q m s 'Query)
anonymousQueryParser = do
  r <- parens '{' '}' queryParser
  pure $ Gql { query    = M.singleton "query" (ANil, r)
             , mutation = M.empty
             }


------------------------------------------------------------------------------
-- |
class GPermFieldsParser (rq :: * -> *) where
  gPermFieldsParser :: [ReaderT Vars Parser (rq x)]

instance  GPermFieldsParser fq => GPermFieldsParser (M1 D b fq) where
  gPermFieldsParser = fmap M1 <$> gPermFieldsParser

instance  GPermFieldsParser fq => GPermFieldsParser (M1 C b fq) where
  gPermFieldsParser = fmap M1 <$> gPermFieldsParser



class GPermTermParser (t :: *) where
  gPermTermParser :: String -> [ReaderT Vars Parser t]

instance (KnownSymbol name, GPermTermParser t)
      => GPermFieldsParser (M1 S ('MetaSel ('Just name) _1 _2 _3)
                                 (K1 _4 t)) where
  gPermFieldsParser = fmap (M1 . K1) <$> gPermTermParser (symbolVal $ Proxy @name)

instance (ParseArgs args, IsAllMaybe args)
      => GPermTermParser (M.Map Text (Args args, ())) where
  gPermTermParser name = pure $ do
    alias <- lift $ try $ parseIdentOrAlias name
    lift skipCrap
    args <- parseOptionalArgs @args
    directiveCombinator alias args $ pure ()

instance  GPermTermParser (Magic 'Query t) => GPermTermParser (ToMagic 'Query t) where
  gPermTermParser name = fmap ToMagic <$> gPermTermParser @(Magic 'Query t) name

instance ( HasQueryParser t
         , ParseArgs args
         , IsAllMaybe args
         ) => GPermTermParser (M.Map Text (Args args, t 'Query)) where
  gPermTermParser name = pure $ do
    alias <- lift $ try $ parseIdentOrAlias name
    lift skipCrap
    args <- parseOptionalArgs @args
    directiveCombinator alias args $ parens '{' '}' $ queryParser @t

instance ( ParseArgs args
         , IsAllMaybe args
         , GQueryParser (M1 _5 _6 _7)
         ) => GPermTermParser (M.Map Text (Args args, (M1 _5 _6 _7) Void)) where
  gPermTermParser name = pure $ do
    alias <- lift $ try $ parseIdentOrAlias name
    lift skipCrap
    args <- parseOptionalArgs @args
    directiveCombinator alias args $ parens '{' '}' $ gQueryParser



instance ( GPermFieldsParser fq
         , GPermFieldsParser gq
         , forall x. (Monoid (fq x), Monoid (gq x))
         ) => GPermFieldsParser (fq :*: gq) where
  -- TODO(sandy): free perf gains here
  gPermFieldsParser =
      (fmap (:*: mempty) <$> gPermFieldsParser @fq)
      ++ (fmap (mempty :*:) <$> gPermFieldsParser @gq)

directiveCombinator :: k
                    -> a
                    -> ReaderT Vars Parser b
                    -> ReaderT Vars Parser (M.Map k (a, b))
directiveCombinator alias args p = do
    include <- optional parseSkipInclude
    z <- p
    case include of
      Just False -> pure $ M.empty
      _          -> pure $ M.singleton alias (args, z)

parseSkipInclude :: ReaderT Vars Parser Bool
parseSkipInclude = parseDirective "include" id <|> parseDirective "skip" not

parseDirective :: String -> (Bool -> Bool) -> ReaderT Vars Parser Bool
parseDirective s f = try $ do
  _     <- char '@'
  _     <- string $ T.pack s
  bool  <- parens '(' ')' $ parseAnArg "if"
  pure $ f bool

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
parseArgList = parens '(' ')' $ runPermutations parseArgs


------------------------------------------------------------------------------
-- |
class ParseArgs (args :: [(Symbol, *)]) where
  parseArgs :: Permutation (ReaderT Vars Parser) (Args args)

instance ParseArgs '[] where
  parseArgs = pure ANil

instance {-# OVERLAPPING #-}
         ( IsArgType t
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

instance ( IsArgType t
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
foldManyOf
    :: (Foldable t, MonadPlus f, MonadParsec e s f, Monoid m)
    => t (f m)
    -> f m
foldManyOf = fmap fold . many . asum

