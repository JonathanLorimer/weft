module Weft.Generics.EmptyQuery
  ( emptyQuery
  , HasEmptyQuery
  ) where

import qualified Data.Map as M
import           Data.Text (Text)
import           GHC.Generics
import           Weft.Internal.Types


type HasEmptyQuery record =
  ( GEmptyQuery (Rep (record 'Query))
  , Generic (record 'Query)
  )

emptyQuery :: forall record . HasEmptyQuery record => record 'Query
emptyQuery = to $ gEmptyQuery @(Rep (record 'Query))

class GEmptyQuery (fq :: * -> *) where
    gEmptyQuery :: fq x

instance (GEmptyQuery fq, GEmptyQuery gq) => GEmptyQuery (fq :*: gq) where
    gEmptyQuery = (gEmptyQuery @fq) :*: (gEmptyQuery @gq)

instance GEmptyQuery fq => GEmptyQuery (M1 x y fq) where
    gEmptyQuery = M1 $ gEmptyQuery @fq

-- | Q3
instance GEmptyQuery (K1 x Bool) where
    gEmptyQuery = K1 False

-- | Q2
instance GEmptyQuery (K1 x (M.Map Text a)) where
    gEmptyQuery = K1 M.empty

