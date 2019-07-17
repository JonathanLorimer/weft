module SchemaGenerator where

import Args
import GHC.Generics
import GHC.TypeLits
import Data.Proxy
import Data.Typeable

data TypeState = Query | Data | Schema | Response

data NameType = NameType
  { ntName :: String
  , ntType :: String
  } deriving (Eq, Ord, Show)

data Field = Field
  { fNameType :: NameType
  , fArgs     :: [NameType]
  } deriving (Show)

class ReifyArgs (args :: [(Symbol, *)]) where
  reifyArgs :: [NameType]

instance ReifyArgs '[] where
  reifyArgs = []

instance (Typeable t, KnownSymbol n, ReifyArgs args) => ReifyArgs ('(n, t) ': args) where
  reifyArgs = reifyNameType @n @t : reifyArgs @args

reifyNameType :: forall n t. (Typeable t, KnownSymbol n) => NameType
reifyNameType = NameType (symbolVal $ Proxy @n)
                         (show $ typeRep $ Proxy @t)

type family ConsFirst (a :: k1) (b :: ([k1], k2)) :: ([k1], k2) where
  ConsFirst a '(b, c) = '(a ': b, c)

type family UnravelArgs (t :: *) :: ([(Symbol, *)], *) where
  UnravelArgs (Arg n t -> a) = ConsFirst '(n, t) (UnravelArgs a)
  UnravelArgs a              = '( '[], a)

type family ToNestedTuple (ts :: [(k1, *)]) :: * where
  ToNestedTuple '[] = ()
  ToNestedTuple ('(a, b) ': ts) = (b, ToNestedTuple ts)

type family Something (u :: ([(Symbol, *)], *)) :: * where
  Something '( '[], r) = r
  Something '( ts,  r) = (ToNestedTuple ts, r)


type family Magic (ts :: TypeState) a where
             Magic 'Data     (Arg n t -> a)     = Magic 'Data a
             Magic 'Data     a                  = a
             Magic 'Query    ts                 = Maybe (Something (UnravelArgs ts))
             Magic 'Response (Arg n t -> a)     = Magic 'Response a
{- R1. -}    Magic 'Response [record 'Response] = Maybe [record 'Response]
{- R2. -}    Magic 'Response (record 'Response) = Maybe (record 'Response)
{- R3. -}    Magic 'Response scalar             = Maybe scalar
             Magic 'Schema   a                  = Field

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

instance {-# OVERLAPPING #-} (KnownSymbol a, Typeable t, ReifyArgs args)
      => GHasSchema (M1 S ('MetaSel ('Just a) b c d) (Rec0 t))
                    (M1 S ('MetaSel ('Just a) b c d) (Rec0 (Field))) where
    gSchema = M1 $ K1 $ Field (reifyNameType @a @t) (reifyArgs @args)

instance (GHasSchema fi fo)
      => GHasSchema (M1 x y fi)
                    (M1 x y fo) where
    gSchema = M1 $ gSchema @fi

