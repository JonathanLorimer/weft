module Parser where

import TestData
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Proxy
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           SchemaGenerator


class GIncrParser rep fq where
  gIncrParser :: (rep x -> fq x) -> (fq x -> rep x -> rep x) -> rep x -> Parser (rep x)

instance {-# OVERLAPPABLE #-} GIncrParser rep fq => GIncrParser rep (M1 a b fq) where
  gIncrParser get set rep = gIncrParser (unM1 . get) (\a -> set (M1 a)) rep

fstG :: (f :*: g) x -> f x
fstG (f :*: _) = f

sndG :: (f :*: g) x -> g x
sndG (_ :*: g) = g

type HasIncrParser record =
     ( Generic (record 'Query)
     , GIncrParser (Rep (record 'Query))
                   (Rep (record 'Query))
     )

incrParser :: HasIncrParser record => record 'Query -> Parser (record 'Query)
incrParser = fmap to . gIncrParser id const . from

instance (GIncrParser rep fq, GIncrParser rep gq) => GIncrParser rep (fq :*: gq) where
  gIncrParser get set rep = do
    rep' <- gIncrParser (fstG . get) (\a b -> set (a :*: sndG (get b)) b) rep <|> pure rep
    rep'' <- gIncrParser (sndG . get) (\a b -> set (fstG (get b) :*: a) b) rep' <|> pure rep'
    gIncrParser (fstG . get) (\a b -> set (a :*: sndG (get b)) b) rep'' <|> pure rep''


instance KnownSymbol name => GIncrParser rep (M1 S ('MetaSel ('Just name) _1 _2 _3) (K1 _4 Bool)) where
  gIncrParser get set rep = do
    string $ T.pack $ symbolVal $ Proxy @name
    skipSpace
    pure $ set (M1 $ K1 True) $ rep

instance (KnownSymbol name, HasIncrParser t) => GIncrParser rep (M1 S ('MetaSel ('Just name) _1 _2 _3) (K1 _4 (Maybe (t 'Query)))) where
  gIncrParser get set rep = do
    string $ T.pack $ symbolVal $ Proxy @name
    skipSpace
    _ <- char '{'
    skipSpace
    z <- incrParser @t undefined
    _ <- char '}'
    skipSpace
    pure $ set (M1 $ K1 $ Just z) $ rep


