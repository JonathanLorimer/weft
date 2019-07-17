module Args where

import Data.Proxy
import GHC.TypeLits
import GHC.OverloadedLabels


type family Sort xs where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)

type family Insert x xs where
  Insert x '[] = x ': '[]
  Insert x (y ': ys) = Insert' (CmpSymbol x y) x y ys

type family Insert' b x y ys where
  Insert' 'LT  x y ys = x ': (y ': ys)
  Insert' _    x y ys = y ': Insert x ys

data Label (s :: Symbol) where
  Label :: KnownSymbol s => Label s

instance (KnownSymbol s', s ~ s') => IsLabel s (Label s') where
  fromLabel = Label


data Args (ts :: [(Symbol, *)]) where
  ANil :: Args '[]
  (:@@) :: Pair s t -> Args ts -> Args ('(s, t) ': ts)
infixr 5 :@@

instance Show (Args '[]) where
  show _ = ""

instance (Show t, KnownSymbol name, Show (Args args)) => Show (Args ('(name, t) ': args)) where
  show ((Label :> v) :@@ args) = mconcat
    [ symbolVal $ Proxy @name
    , "="
    , show v
    , " :@@ "
    , show args
    ]



data Pair (s :: Symbol) (t :: *) where
  (:>) :: Label s -> t -> Pair s t
infixr 6 :>

