module Args where

import Data.Proxy
import GHC.TypeLits
import Data.Kind

data Arg (name :: Symbol) a = KnownSymbol name => Arg { getArg :: a }

deriving instance Eq t => Eq (Arg n t)
deriving instance Show t => Show (Arg n t)
deriving instance Ord t => Ord (Arg n t)


type family AllHave (c :: * -> Constraint) (ts :: [(Symbol, *)]) :: Constraint where
  AllHave c '[] = (() :: Constraint)
  AllHave c ('(n, t) ': args) = (c t, AllHave c args)


data Args (ts :: [(Symbol, *)]) where
  ANil :: Args '[]
  (:@@) :: Arg s t -> Args ts -> Args ('(s, t) ': ts)
infixr 5 :@@


elimArgs
    :: forall c m ts
     . ( Monoid m
       , AllHave c ts
       )
    => (forall n t. c t => Arg n t -> m)
    -> Args ts
    -> m
elimArgs _ ANil = mempty
elimArgs f (arg :@@ args) = mappend (f arg) $ elimArgs @c f args


instance Show (Args '[]) where
  show _ = ""

instance (Show t, KnownSymbol name, Show (Args args)) => Show (Args ('(name, t) ': args)) where
  show (Arg v :@@ args) = mconcat
    [ symbolVal $ Proxy @name
    , "="
    , show v
    , " :@@ "
    , show args
    ]

