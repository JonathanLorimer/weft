module Weft.Internal.ArgTypes where

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
import qualified Data.Text as T
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits hiding (ErrorMessage (..))
import           Prelude hiding ((<>))
import           Text.Megaparsec hiding (empty)
import qualified Text.Megaparsec.Char as MP
import           Text.Megaparsec.Char hiding (char)
import           Text.Megaparsec.Char.Lexer (charLiteral)
import qualified Text.PrettyPrint.HughesPJ as PPR
import           Text.PrettyPrint.HughesPJ hiding (char, empty, parens)
import           Weft.Internal.ParserUtils
import           Weft.Internal.Types



------------------------------------------------------------------------------
-- |
class IsArgType a where
  pprArg :: Arg n a -> Doc
  parseArgValue :: ReaderT Vars Parser a

instance IsArgType Integer where
  pprArg (Arg v :: Arg n Integer) = pprArgWithName @n $ show v
  parseArgValue = lift $ do
    neg <- optional $ MP.char '-'
    num <- some digitChar
    pure $ read $ maybe "" pure neg ++ num

instance IsArgType Int where
  pprArg (Arg v :: Arg n Int) = pprArgWithName @n $ show v
  parseArgValue = fromInteger <$> parseArgValue

instance IsArgType Double where
  pprArg (Arg v :: Arg n Double) = pprArgWithName @n $ show v
  parseArgValue = lift $ do
    neg <- optional $ MP.char '-'
    num <- some digitChar
    dec <- optional $ do
      _ <- MP.char '.'
      ('.' :) <$> some digitChar
    pure $ read $ maybe "" pure neg ++ num ++ fromMaybe "" dec

instance IsArgType Bool where
  pprArg (Arg v :: Arg n Bool) = pprArgWithName @n $ fmap toLower $ show v
  parseArgValue = lift $ asum
    [ False <$ string "false"
    , True  <$ string "true"
    ]

instance IsArgType String where
  pprArg (Arg v :: Arg n String) = pprArgWithName @n $ show v
  parseArgValue = lift $ MP.char '"' >> manyTill charLiteral (MP.char '"')

instance IsArgType ID where
  pprArg (Arg (ID v) :: Arg n ID) = pprArgWithName @n $ show v
  parseArgValue = asum
    [ ID . show <$> parseArgValue @Integer
    , ID <$> parseArgValue @String
    ]

instance {-# OVERLAPPABLE #-} (Generic a, GIsArgThing (Rep a))
      => IsArgType a where
  pprArg (Arg v :: Arg n a) =
    sep [ text (symbolVal $ Proxy @n) <> PPR.char ':'
        , gPprThing $ from v
        ]
  parseArgValue = fmap to gParseThing

instance {-# OVERLAPPING #-} IsArgType t => IsArgType (Maybe t) where
  pprArg (Arg Nothing) = PPR.empty
  pprArg (Arg (Just v) :: Arg n (Maybe t)) = pprArg (Arg @n v)
  parseArgValue = error "sandy: i don't think this is necessary"


------------------------------------------------------------------------------
-- |
class GIsArgThing (rq :: * -> *) where
  gPprThing :: rq x -> Doc
  gParseThing :: ReaderT Vars Parser (rq x)

instance GIsArgThing f => GIsArgThing (M1 _1 _2 f) where
  gPprThing (M1 f) = gPprThing f
  gParseThing = M1 <$> gParseThing

instance GIsInput (f :*: g) => GIsArgThing (f :*: g) where
  gPprThing z = vcat
    [ PPR.char '{'
    , nest 4 $ gPprInput z
    , PPR.char '}'
    ]
  gParseThing = do
    parens '{' '}' $ runPermutations gParseInput

instance GIsEnum (f :+: g) => GIsArgThing (f :+: g) where
  gPprThing = gPprEnum
  gParseThing = lift gParseEnum


------------------------------------------------------------------------------
-- |
class GIsEnum rq where
  gPprEnum :: rq x -> Doc
  gParseEnum :: Parser (rq x)

instance (GIsEnum f, GIsEnum g) => GIsEnum (f :+: g) where
  gPprEnum (L1 f) = gPprEnum f
  gPprEnum (R1 g) = gPprEnum g
  gParseEnum = L1 <$> gParseEnum <|> R1 <$> gParseEnum

instance KnownSymbol name => GIsEnum (M1 C ('MetaCons name _1 _2) U1) where
  gPprEnum (M1 U1) = text $ fmap toUpper $ symbolVal $ Proxy @name
  gParseEnum =
    M1 U1 <$ (string $ T.pack $ fmap toUpper $ symbolVal $ Proxy @name)


class GIsInput rq where
  gPprInput :: rq x -> Doc
  gParseInput :: Permutation (ReaderT Vars Parser) (rq x)

instance (GIsInput f, GIsInput g) => GIsInput (f :*: g) where
  gPprInput (f :*: g) =
    vcat $ punctuate (PPR.char ',') $ filter (not . isEmpty)
      [ gPprInput f
      , gPprInput g
      ]
  gParseInput = (:*:) <$> gParseInput <*> gParseInput

instance ( KnownSymbol name
         , IsArgType t
         , Typeable t
         ) => GIsInput (M1 S ('MetaSel ('Just name) b c d) (K1 x t)) where
  gPprInput (M1 (K1 t)) = pprArg $ Arg @name t
  gParseInput =
    fmap (M1 . K1) $ toPermutation $ parseAnArg $ symbolVal $ Proxy @name

instance {-# OVERLAPPING #-} ( KnownSymbol name
         , IsArgType t
         , Typeable t
         ) => GIsInput (M1 S ('MetaSel ('Just name) b c d)
                             (K1 x (Maybe t))) where
  gPprInput (M1 (K1 t)) = pprArg $ Arg @name t
  gParseInput =
    fmap (M1 . K1) . toPermutationWithDefault Nothing
                   . fmap Just
                   . parseAnArg
                   . symbolVal
                   $ Proxy @name


------------------------------------------------------------------------------
-- |
parseAnArg :: (Typeable a, IsArgType a) => String -> ReaderT Vars Parser a
parseAnArg arg_name = do
  lift skipCrap
  _ <- lift $ string $ T.pack arg_name
  lift skipCrap
  _ <- lift $ MP.char ':'
  lift skipCrap
  result <- parseRawArgValue
  lift skipCrap
  pure result


parseRawArgValue :: forall a. (Typeable a, IsArgType a) => ReaderT Vars Parser a
parseRawArgValue = choice
  [ do
      ident <- lift $ MP.char '$' *> parseAnIdentifier
      vars <- ask
      case M.lookup ident vars of
        Just res ->
          case parseMaybe (runReaderT parseArgValue vars) (T.pack res) of
            Just a -> pure a
            Nothing -> failure Nothing
                     . S.singleton
                     . Label
                     . NE.fromList
                     . ("value that should have parsed as: " ++)
                     . show
                     . typeRep
                     $ Proxy @a
        Nothing -> lift . failure (Just $ wrapLabel ident)
                        . S.fromList
                        . fmap wrapLabel
                        $ M.keys vars
  , parseArgValue <* lift skipCrap
  ]


pprArgWithName :: forall n. KnownSymbol n => String -> Doc
pprArgWithName val =
    sep [ text (symbolVal $ Proxy @n) <> PPR.char ':'
        , text val
        ]

