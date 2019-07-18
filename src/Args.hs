module Args where

import Data.Proxy
import GHC.TypeLits
import Data.Kind
import Test.QuickCheck (Arbitrary (..), oneof, suchThat)
import Data.Maybe

class IsAllMaybe (args :: [(Symbol, *)]) where
  isAllMaybe :: Maybe (Args args)

instance IsAllMaybe '[] where
  isAllMaybe = Just ANil

instance {-# OVERLAPPING #-} (KnownSymbol a, IsAllMaybe ts) => IsAllMaybe ('(a, Maybe b) ': ts) where
  isAllMaybe = (:@@) <$> (Just $ Arg Nothing) <*> isAllMaybe @ts

instance IsAllMaybe ('(a, b) ': ts) where
  isAllMaybe = Nothing

data Arg (name :: Symbol) a = KnownSymbol name => Arg { getArg :: a }

instance (KnownSymbol name, Arbitrary t) => Arbitrary (Arg name t) where
  arbitrary = Arg <$> arbitrary

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

instance Eq (Args '[]) where
  ANil == ANil = True

instance (Eq t, Eq (Args args)) => Eq (Args ('(name, t) ': args)) where
  (Arg a :@@ args) == (Arg b :@@ args') = a == b && args == args'


instance Arbitrary (Args '[]) where
  arbitrary = pure ANil

instance (KnownSymbol name, Arbitrary t, Arbitrary (Args args)) => Arbitrary (Args ('(name, t) ': args)) where
  arbitrary = (:@@) <$> arbitrary <*> arbitrary

instance {-# OVERLAPPING #-} (IsAllMaybe args, Arbitrary (Args args), Arbitrary t) => Arbitrary (Maybe (Args args, t)) where
  arbitrary = oneof
    [ pure Nothing
    , fmap Just $ (,) <$> arbitrary <*> arbitrary
    ] `suchThat` maybe (isJust $ isAllMaybe @args) (const True)




