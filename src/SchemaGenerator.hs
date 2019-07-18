module SchemaGenerator where

import Args
import GHC.Generics
import GHC.TypeLits
import Data.Proxy
import Data.Typeable
import Data.List.NonEmpty

data TypeState = Query | Data | Schema | Response | Resolver

data NameType = NameType
  { ntName :: String
  , ntType :: GqlType
  } deriving (Eq, Ord, Show)

data GqlType
  = GqlList   Bool GqlType
  | GqlSingle Bool String
  deriving (Eq, Ord, Show)

class HasGqlType a where
  gqlType :: GqlType

instance {-# OVERLAPPING #-} HasGqlType String where
  gqlType = GqlSingle True "String"

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


data Field args = Field
  { fNameType :: NameType
  , fArgs     :: [NameType]
  } deriving (Show)

class ReifyArgs (args :: [(Symbol, *)]) where
  reifyArgs :: [NameType]

instance ReifyArgs '[] where
  reifyArgs = []

instance (HasGqlType t, KnownSymbol n, ReifyArgs args) => ReifyArgs ('(n, t) ': args) where
  reifyArgs = reifyNameType @n @t : reifyArgs @args

reifyNameType :: forall n t. (HasGqlType t, KnownSymbol n) => NameType
reifyNameType = NameType (symbolVal $ Proxy @n) $ gqlType @t

type family ConsFirst (a :: k1) (b :: ([k1], k2)) :: ([k1], k2) where
  ConsFirst a '(b, c) = '(a ': b, c)

type family UnravelArgs (t :: *) :: ([(Symbol, *)], *) where
  UnravelArgs (Arg t n -> a) = ConsFirst '(t, n) (UnravelArgs a)
  UnravelArgs a        = '( '[], a)

type family Something (u :: ([(Symbol, *)], *)) :: * where
  Something '(ts, [record 'Query]) = (Args ts, record 'Query)
  Something '(ts, record 'Query)   = (Args ts, record 'Query)
  Something '(ts, a)               = (Args ts, ())

type family Fst (u :: (k1, k2)) :: k1 where
  Fst '(ts, a) = ts


type family Magic (ts :: TypeState) a where
  Magic 'Resolver (Arg n t -> a)     = Arg n t -> Magic 'Resolver a
  Magic 'Resolver [record 'Resolver] = record 'Query -> IO [record 'Response]
  Magic 'Resolver (record 'Resolver) = record 'Query -> IO (record 'Response)
  Magic 'Resolver a                  = IO a
  Magic 'Data     (Arg n t -> a)     = Magic 'Data a
  Magic 'Data     a                  = a
  Magic 'Query    ts                 = Maybe (Something (UnravelArgs ts))
  Magic 'Response (Arg n t -> a)     = Magic 'Response a
  Magic 'Response [record 'Response] = Maybe [record 'Response]  -- R1
  Magic 'Response (record 'Response) = Maybe (record 'Response)  -- R2
  Magic 'Response scalar             = Maybe scalar              -- R3
  Magic 'Schema   ts                 = Field (Fst (UnravelArgs ts))

-- | Schema Generation
schema
    :: forall record
     . ( Generic (record 'Schema)
       , GHasSchema (Rep (record 'Data))
                    (Rep (record 'Schema))
       , Generic (record 'Data)
       )
    => record 'Schema
schema = to $ gSchema @(Rep (record 'Data))

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

