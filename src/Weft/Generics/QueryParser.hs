module Weft.Generics.QueryParser
  ( HasQueryParser
  , queryParser
  ) where

import           Control.Applicative
import           Control.Applicative.Permutations
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Char
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

incrParser :: HasQueryParser record => Parser (record 'Query)
incrParser = fmap to gQueryParser


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

instance (KnownSymbol name, ParseArgs args, IsAllMaybe args)
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
         , ParseArgs args
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
class GQueryParser (rq :: * -> *) where
  gQueryParser :: Parser (rq x)

instance {-# OVERLAPPABLE #-} GQueryParser fq => GQueryParser (M1 a b fq) where
  gQueryParser = M1 <$> gQueryParser

instance ( GPermFieldsParser (fq :*: gq)) => GQueryParser (fq :*: gq) where
  gQueryParser = intercalateEffect skipSpace gPermFieldsParser

instance GPermFieldsParser (M1 _1 _2 (K1 _3 f)) => GQueryParser (M1 _1 _2 (K1 _3 f)) where
  gQueryParser = runPermutation gPermFieldsParser


------------------------------------------------------------------------------
-- |
parseOptionalArgs :: forall args. (ParseArgs args, IsAllMaybe args) => Parser (Args args)
parseOptionalArgs =
  case isAllMaybe @args of
    Nothing -> parseArgList
    Just argsOfNothing ->
      fmap (fromMaybe argsOfNothing)
        $ optional parseArgList

parseArgList :: ParseArgs args => Parser (Args args)
parseArgList = do
  _ <- char '('
  z <- intercalateEffect (skipSpace >> char ',' >> skipSpace) $ parseArgs
  _ <- char ')'
  pure z


parseAnArg :: Read a => String -> Parser a
parseAnArg arg_name = do
  skipSpace
  _ <- string $ BS.pack arg_name
  skipSpace
  _ <- char ':'
  skipSpace
  -- TODO(sandy): make this less shitty
  result <- parseRawArgValue
  pure $ read result


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


------------------------------------------------------------------------------
-- |
class ParseArgs (args :: [(Symbol, *)]) where
  parseArgs :: Permutation Parser (Args args)

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

