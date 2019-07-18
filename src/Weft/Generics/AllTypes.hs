module Weft.Generics.AllTypes
  ( HasAllTypes
  , allTypes
  ) where

import           Data.List.NonEmpty
import qualified Data.Map as M
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics
import           Prelude hiding ((<>))
import           Text.PrettyPrint.HughesPJ
import           Weft.Generics.Schema
import           Weft.Generics.PprSchema
import           Weft.Types


------------------------------------------------------------------------------
-- |
type HasAllTypes record =
  ( GFindTypes (Rep (record 'Data))
  , HasPprSchema record
  , HasSchema record
  , Typeable record
  )


------------------------------------------------------------------------------
-- |
allTypes :: forall record. HasAllTypes record => [Doc]
allTypes = fmap snd $ M.toList $ findTypes @record M.empty


------------------------------------------------------------------------------
-- |
findTypes
    :: forall record
     . HasAllTypes record
    => M.Map TypeRep Doc
    -> M.Map TypeRep Doc
findTypes m =
  case M.lookup name m of
    Just _  -> m
    Nothing -> gFindTypes @(Rep (record 'Data)) $ M.insert name (pprSchema $ schema @record) m
  where
    name = typeRep $ Proxy @record


------------------------------------------------------------------------------
-- |
class GFindTypes rd where
  gFindTypes :: M.Map TypeRep Doc -> M.Map TypeRep Doc

instance GFindTypes rd => GFindTypes (M1 _1 _2 rd) where
  gFindTypes = gFindTypes @rd

instance (GFindTypes rd, GFindTypes rd') => GFindTypes (rd :*: rd') where
  gFindTypes m =
    let m' = gFindTypes @rd m
     in gFindTypes @rd' m'

instance (HasAllTypes record) => GFindTypes (K1 _1 (record 'Data)) where
  gFindTypes = findTypes @record

instance (HasAllTypes record) => GFindTypes (K1 _1 [record 'Data]) where
  gFindTypes = findTypes @record

instance (HasAllTypes record) => GFindTypes (K1 _1 (NonEmpty (record 'Data))) where
  gFindTypes = findTypes @record

instance {-# OVERLAPPABLE #-} GFindTypes (K1 _1 _2) where
  gFindTypes = id

