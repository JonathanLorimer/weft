{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}

module Weft.Internal.Types
  ( module Weft.Internal.Types
  , HKD (..)
  ) where

import           Data.Aeson
import           Data.Generic.HKD
import           Data.Kind
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import           Data.Void
import           GHC.Generics
import           GHC.TypeLits
import           Test.QuickCheck (Arbitrary (..), suchThat, oneof, resize, sized)
import           Text.Megaparsec


type Vars = M.Map String String
type Parser = Parsec Void Text


------------------------------------------------------------------------------
-- |
data TypeState = Query | Schema | Response | Resolver


------------------------------------------------------------------------------
-- |
type family Magic (ts :: TypeState) a where
  Magic 'Resolver (Method ('(n, t) ': args) a) = Arg n t -> Magic 'Resolver (Method args a)
  Magic 'Resolver (Method '[] a) = Magic 'Resolver a
  Magic 'Resolver a                  = MagicResolve a

  Magic 'Query    t                  = M.Map Text (MagicQueryResult t (UnravelArgs t))

  Magic 'Response (Method args a)    = Magic 'Response a
  Magic 'Response a                  = M.Map Text (MagicResponse a)

  Magic 'Schema   ts                 = Field (Fst (UnravelArgs ts))


type family MagicResolve (a :: *) :: * where
  MagicResolve Int     = IO Int
  MagicResolve Integer = IO Integer
  MagicResolve Double  = IO Double
  MagicResolve Bool    = IO Bool
  MagicResolve String  = IO String
  MagicResolve ID      = IO ID
  MagicResolve ()      = IO ()
  MagicResolve [a]     = J a 'Query Void -> IO [J a 'Response Void]
  MagicResolve a       = J a 'Query Void -> IO (J a 'Response Void)


type family MagicResponse (a :: *) :: * where
  MagicResponse Int     = Int
  MagicResponse Integer = Integer
  MagicResponse Double  = Double
  MagicResponse Bool    = Bool
  MagicResponse String  = String
  MagicResponse ID      = ID
  MagicResponse ()      = ()
  MagicResponse [a]     = [MagicResponse a]
  MagicResponse a       = J a 'Response Void


newtype ToMagic (ts :: TypeState) (a :: *) = ToMagic
  { unMagic :: Magic ts a
  }

pattern ToQuery :: Magic 'Query a -> ToMagic 'Query a
pattern ToQuery a = ToMagic a

pattern ToResolver :: Magic 'Resolver a -> ToMagic 'Resolver a
pattern ToResolver a = ToMagic a

pattern ToResponse :: Magic 'Response a -> ToMagic 'Response a
pattern ToResponse a = ToMagic a

deriving instance Eq (Magic ts a)        => Eq (ToMagic ts a)
deriving instance Semigroup (Magic ts a) => Semigroup (ToMagic ts a)
deriving instance Monoid (Magic ts a)    => Monoid (ToMagic ts a)
deriving instance Arbitrary (Magic ts a) => Arbitrary (ToMagic ts a)

type JHKD (rec :: *) (ts :: TypeState) = HKD rec (ToMagic ts)
type J  (rec :: *) (ts :: TypeState) = HKD_ (ToMagic ts) rec
type J' (rec :: *) (ts :: TypeState) = J rec ts Void

data Method (args :: [(Symbol, *)]) (res :: *) = Method res
  deriving Generic


------------------------------------------------------------------------------
-- |
type family ConsFirst (a :: k1) (b :: ([k1], k2)) :: ([k1], k2) where
  ConsFirst a '(b, c) = '(a ': b, c)

type family UnravelArgs (t :: *) :: ([(Symbol, *)], *) where
  UnravelArgs (Method args a) = '(args, a)
  UnravelArgs a               = '( '[], a)

type family MagicQueryResult (use :: *) (u :: ([(Symbol, *)], *)) :: * where
  MagicQueryResult use '(ts, a) = (Args ts, MagicQueryInputOutput a use)

type family MagicQueryInputOutput (t :: *) (use :: *) :: * where
  MagicQueryInputOutput Int     _ = ()
  MagicQueryInputOutput Integer _ = ()
  MagicQueryInputOutput Double  _ = ()
  MagicQueryInputOutput Bool    _ = ()
  MagicQueryInputOutput String  _ = ()
  MagicQueryInputOutput ID      _ = ()
  MagicQueryInputOutput ()      _ = ()
  MagicQueryInputOutput [a]   use = MagicQueryInputOutput a use
  MagicQueryInputOutput a     use = J a 'Query Void

type family Fst (u :: (k1, k2)) :: k1 where
  Fst '(ts, a) = ts


------------------------------------------------------------------------------
-- |
data Arg (name :: Symbol) a = KnownSymbol name => Arg { getArg :: a }

instance (KnownSymbol name, Arbitrary t) => Arbitrary (Arg name t) where
  arbitrary = Arg <$> arbitrary

deriving instance Eq t => Eq (Arg n t)
deriving instance Show t => Show (Arg n t)
deriving instance Ord t => Ord (Arg n t)


------------------------------------------------------------------------------
-- |
data Args (ts :: [(Symbol, *)]) where
  ANil :: Args '[]
  (:@@) :: Arg s t -> Args ts -> Args ('(s, t) ': ts)
infixr 5 :@@

instance Show (Args '[]) where
  show ANil = "ANil"

instance (Show t, KnownSymbol name, Show (Args args)) => Show (Args ('(name, t) ': args)) where
  show (Arg v :@@ args) = mconcat
    [ "Arg "
    , showsPrec 10 v ""
    , " :@@ "
    , show args
    ]

instance Eq (Args '[]) where
  ANil == ANil = True

instance (Eq t, Eq (Args args)) => Eq (Args ('(name, t) ': args)) where
  (Arg a :@@ args) == (Arg b :@@ args') = a == b && args == args'


instance Arbitrary (Args '[]) where
  arbitrary = pure ANil

instance
      ( KnownSymbol name
      , Arbitrary t
      , Arbitrary (Args args)
      ) => Arbitrary (Args ('(name, t) ': args)) where
  arbitrary = (:@@) <$> arbitrary <*> arbitrary

instance {-# OVERLAPPING #-}
      ( IsAllMaybe args
      , Arbitrary t
      , Arbitrary (Args args)
      ) => Arbitrary (Maybe (Args args, t)) where
  arbitrary = sized $ \case
    0 -> pure Nothing
    n -> resize (n - 1) $ oneof
           [ pure Nothing
           , fmap Just $ (,) <$> arbitrary <*> arbitrary
           ] `suchThat` maybe (isJust $ isAllMaybe @args) (const True)

data None (ts :: TypeState) =
  None { dontTouch :: Magic ts () }
    deriving (Generic)

deriving instance AllHave Eq (None ts) => Eq (None ts)
deriving instance AllHave Show (None ts) => Show (None ts)

data Gql (q :: *)
         (m :: *)
         (s :: TypeState -> *)
         (ts :: TypeState) = Gql
  { query        :: Magic ts q
  , mutation     :: Magic ts m
  -- , subscription :: s ts
  }
  deriving Generic

-- TODO(sandy): delete this crap
type AllHave c a = GFields c (Rep a)

type family GFields (c :: * -> Constraint) (f :: * -> *) :: Constraint
type instance GFields c (M1 i j f) = GFields c f
type instance GFields c (f :+: g)  = (GFields c f, GFields c g)
type instance GFields c (f :*: g)  = (GFields c f, GFields c g)
type instance GFields c U1         = ()
type instance GFields c (K1 i a)   = c a

deriving instance AllHave Show (Gql q m s ts) => Show (Gql q m s ts)
deriving instance AllHave Eq (Gql q m s ts) => Eq (Gql q m s ts)

------------------------------------------------------------------------------
-- |

newtype ID = ID String
  deriving newtype (ToJSON, Arbitrary)
  deriving stock (Eq, Show, Ord)

------------------------------------------------------------------------------
-- |
data NameType = NameType
  { ntName :: String
  , ntType :: GqlType
  } deriving (Eq, Ord, Show)


------------------------------------------------------------------------------
-- |
data GqlType
  = GqlList   Bool GqlType
  | GqlSingle Bool String
  deriving (Eq, Ord, Show)


------------------------------------------------------------------------------
-- |
data Field args = Field
  { fNameType :: NameType
  , fArgs     :: [NameType]
  } deriving (Show, Eq, Ord)

------------------------------------------------------------------------------
-- |

class IsAllMaybe (args :: [(Symbol, *)]) where
  isAllMaybe :: Maybe (Args args)

instance IsAllMaybe '[] where
  isAllMaybe = Just ANil

instance {-# OVERLAPPING #-} (KnownSymbol a, IsAllMaybe ts) => IsAllMaybe ('(a, Maybe b) ': ts) where
  isAllMaybe = (:@@) <$> (Just $ Arg Nothing) <*> isAllMaybe @ts

instance IsAllMaybe ('(a, b) ': ts) where
  isAllMaybe = Nothing

