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
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits hiding (ErrorMessage (..))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Weft.Generics.EmptyQuery
import           Weft.Internal.Types
import           Weft.Internal.ArgTypes
import           Weft.Internal.ParserUtils



type HasQueryParser record =
     ( Generic (record 'Query)
     , GQueryParser (Rep (record 'Query))
     )

queryParser :: HasQueryParser record => ReaderT Vars Parser (record 'Query)
queryParser = lift skipCrap *> fmap to gQueryParser <* lift skipCrap


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
    :: (Functor t, Foldable t, MonadPlus f, MonadParsec e s f, Monoid m)
    => t (f m)
    -> f m
foldManyOf = fmap fold . many . asum

