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

data Field (args :: [(Symbol, *)]) = Field
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


type family Magic (ts :: TypeState) (args :: [(Symbol, *)]) a where
             Magic 'Data     args a                  = a
{- Q1. -}    Magic 'Query    args [record 'Query]    = Maybe (Args args, record 'Query)
{- Q2. -}    Magic 'Query    args (record 'Query)    = Maybe (Args args, record 'Query)
{- Q3. -}    Magic 'Query    args scalar             = Maybe (Args args)
{- R1. -}    Magic 'Response args [record 'Response] = Maybe [record 'Response]
{- R2. -}    Magic 'Response args (record 'Response) = Maybe (record 'Response)
{- R3. -}    Magic 'Response args scalar             = Maybe scalar
             Magic 'Schema   args a                  = Field args

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
                    (M1 S ('MetaSel ('Just a) b c d) (Rec0 (Field args))) where
    gSchema = M1 $ K1 $ Field (reifyNameType @a @t) (reifyArgs @args)

instance (GHasSchema fi fo)
      => GHasSchema (M1 x y fi)
                    (M1 x y fo) where
    gSchema = M1 $ gSchema @fi

