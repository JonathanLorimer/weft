module Weft.Generics.Schema
  ( HasSchema
  , schema
  ) where

import Data.List.NonEmpty
import Data.Proxy
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import Weft.Internal.Types


------------------------------------------------------------------------------
-- |
type HasSchema record =
  ( GHasSchema (Rep (record 'Data))
               (Rep (record 'Schema))
  , Generic (record 'Data)
  , Generic (record 'Schema)
  )


--------------------------------------------------------------------------------
-- |
schema
    :: forall record
     . HasSchema record
    => record 'Schema
schema = to $ gSchema @(Rep (record 'Data))


------------------------------------------------------------------------------
-- |
class GHasSchema i o where
    gSchema :: o x

instance (GHasSchema fi fo, GHasSchema gi go) => GHasSchema (fi :*: gi) (fo :*: go) where
    gSchema = gSchema @fi :*: gSchema @gi

instance {-# OVERLAPPING #-} (KnownSymbol a, HasGqlType t, ReifyArgs args)
      => GHasSchema (M1 S ('MetaSel ('Just a) b c d) (Rec0 t))
                    (M1 S ('MetaSel ('Just a) b c d) (Rec0 (Field args))) where
    gSchema = M1 $ K1 $ Field (reifyNameType @a @t) (reifyArgs @args)

instance (GHasSchema fi fo)
      => GHasSchema (M1 x y fi)
                    (M1 x y fo) where
    gSchema = M1 $ gSchema @fi


------------------------------------------------------------------------------
-- |
reifyNameType :: forall n t. (HasGqlType t, KnownSymbol n) => NameType
reifyNameType = NameType (symbolVal $ Proxy @n) $ gqlType @t


------------------------------------------------------------------------------
-- |
class HasGqlType a where
  gqlType :: GqlType

instance {-# OVERLAPPING #-} HasGqlType String where
  gqlType = GqlSingle True "String"

instance {-# OVERLAPPING #-} Typeable record => HasGqlType (record (ts :: TypeState)) where
  gqlType = GqlSingle True $ show $ typeRep $ Proxy @record

instance {-# OVERLAPPABLE #-} Typeable a => HasGqlType a where
  gqlType = GqlSingle True $ show $ typeRep $ Proxy @a

instance HasGqlType a => HasGqlType [a] where
  gqlType = GqlList False $ gqlType @a

instance HasGqlType a => HasGqlType (NonEmpty a) where
  gqlType = GqlList True $ gqlType @a

instance HasGqlType a => HasGqlType (Maybe a) where
  gqlType =
    case gqlType @a of
      GqlList   _ t -> GqlList False t
      GqlSingle _ t -> GqlSingle False t


------------------------------------------------------------------------------
-- |
class ReifyArgs (args :: [(Symbol, *)]) where
  reifyArgs :: [NameType]

instance ReifyArgs '[] where
  reifyArgs = []

instance (HasGqlType t, KnownSymbol n, ReifyArgs args) => ReifyArgs ('(n, t) ': args) where
  reifyArgs = reifyNameType @n @t : reifyArgs @args

