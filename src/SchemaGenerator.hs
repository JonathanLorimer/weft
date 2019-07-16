module SchemaGenerator where

import GHC.Generics
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Proxy
import Data.Typeable

data TypeState = Query | Data | Schema | Response

data SchemaRecord = SchemaRecord { fieldName :: String
                                 , fieldType :: String } deriving (Show)

data Label (s :: Symbol) where
  Label :: KnownSymbol s => Label s

instance (KnownSymbol s', s ~ s') => IsLabel s (Label s') where
  fromLabel = Label


data Args (ts :: [(Symbol, *)]) where
  ANil :: Args '[]
  (:@@) :: Pair s t -> Args ts -> Args ('(s, t) ': ts)
infixr 5 :@@


data Pair (s :: Symbol) (t :: *) where
  (:~>) :: Label s -> t -> Pair s t
infixr 6 :~>


type family Magic (ts :: TypeState) a where
             Magic 'Data a = a
{- Q1. -}    Magic 'Query [record 'Query] = Maybe (record 'Query)
{- Q2. -}    Magic 'Query (record 'Query) = Maybe (record 'Query)
{- Q3. -}    Magic 'Query field = Bool
{- R1. -}    Magic 'Response [record 'Response] = Maybe [record 'Response]
{- R2. -}    Magic 'Response (record 'Response) = Maybe (record 'Response)
{- R3. -}    Magic 'Response field = Maybe field
             Magic 'Schema a = SchemaRecord

-- | Schema Generation
schema :: forall record . (Generic (record 'Schema), GHasSchema (Rep (record 'Data)) (Rep (record 'Schema)), Generic (record 'Data)) => record 'Schema
schema = to $ gSchema @(Rep (record 'Data))

class GHasSchema i o where
    gSchema :: o x

instance (GHasSchema fi fo, GHasSchema gi go) => GHasSchema (fi :*: gi) (fo :*: go) where
    gSchema = gSchema @fi :*: gSchema @gi

instance {-# OVERLAPPING #-}(KnownSymbol a, Typeable t)
      => GHasSchema (M1 S ('MetaSel ('Just a) b c d) (Rec0 t))
                    (M1 S ('MetaSel ('Just a) b c d) (Rec0 SchemaRecord)) where
    gSchema = M1 $ K1 $ SchemaRecord (symbolVal $ Proxy @a) $ show $ typeRep $ Proxy @t

instance (GHasSchema fi fo)
      => GHasSchema (M1 x y fi)
                    (M1 x y fo) where
    gSchema = M1 $ gSchema @fi
