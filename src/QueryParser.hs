module QueryParser where

import TestData
import GHC.Generics
import SchemaGenerator


emptyQuery :: forall record . ( GEmptyQuery (Rep (record 'Query))
              , Generic (record 'Query)
              ) => record 'Query
emptyQuery = to $ gEmptyQuery @(Rep (record 'Query))

class GEmptyQuery (fq :: * -> *) where
    gEmptyQuery :: fq x

instance (GEmptyQuery fq, GEmptyQuery gq) => GEmptyQuery (fq :*: gq) where
    gEmptyQuery = (gEmptyQuery @fq) :*: (gEmptyQuery @gq)

instance GEmptyQuery fq => GEmptyQuery (M1 x y fq) where
    gEmptyQuery = M1 $ gEmptyQuery @fq

-- | Q3
instance GEmptyQuery (K1 x (Bool)) where
    gEmptyQuery = K1 $ False

-- | Q2
instance GEmptyQuery (K1 x (Maybe (record 'Query))) where
    gEmptyQuery = K1 $ Nothing


-- $> emptyQuery @User'