{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Weft.Internal.Types where

import           Data.Aeson
import           Data.Kind
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import           Data.Void
import           GHC.Generics
import           GHC.TypeLits hiding (ErrorMessage (..))
import           Lens.Micro ((^?))
import           Lens.Micro.Aeson
import           Test.QuickCheck (Arbitrary (..), suchThat, oneof, resize, sized)
import           Text.Megaparsec


type Vars = M.Map String String
type Parser = Parsec Void Text


------------------------------------------------------------------------------
-- |
data TypeState = Query | Data | Schema | Response | Resolver


------------------------------------------------------------------------------
-- |
type family Magic (ts :: TypeState) a where
  Magic 'Resolver (Arg n t -> a)     = Arg n t -> Magic 'Resolver a               -- RV1
  Magic 'Resolver [record 'Resolver] = record 'Query -> IO [record 'Response]     -- RV2
  Magic 'Resolver (record 'Resolver) = record 'Query -> IO (record 'Response)     -- RV3
  Magic 'Resolver a                  = IO a                                       -- RV4

  Magic 'Data     (Arg n t -> a)     = Magic 'Data a                              -- D1
  Magic 'Data     a                  = a                                          -- D2

  Magic 'Query    ts                 = M.Map Text (MagicQueryResult (UnravelArgs ts))

  Magic 'Response (Arg n t -> a)     = Magic 'Response a
  Magic 'Response [record 'Response] = M.Map Text [record 'Response]                   -- RP1
  Magic 'Response (record 'Response) = M.Map Text (record 'Response)                   -- RP2
  Magic 'Response scalar             = M.Map Text scalar                               -- RP3

  Magic 'Schema   ts                 = Field (Fst (UnravelArgs ts))


------------------------------------------------------------------------------
-- |
type family ConsFirst (a :: k1) (b :: ([k1], k2)) :: ([k1], k2) where
  ConsFirst a '(b, c) = '(a ': b, c)

type family UnravelArgs (t :: *) :: ([(Symbol, *)], *) where
  UnravelArgs (Arg t n -> a) = ConsFirst '(t, n) (UnravelArgs a)
  UnravelArgs a        = '( '[], a)

type family MagicQueryResult (u :: ([(Symbol, *)], *)) :: * where
  MagicQueryResult '(ts, [record 'Query]) = (Args ts, record 'Query)
  MagicQueryResult '(ts, record 'Query)   = (Args ts, record 'Query)
  MagicQueryResult '(ts, a)               = (Args ts, ())

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


data Gql (q :: TypeState -> *)
         (m :: TypeState -> *)
         (s :: TypeState -> *)
         (ts :: TypeState) = Gql
  { query        :: Magic ts (q ts)
  -- , mutation     :: m ts
  -- , subscription :: s ts
  }
  deriving Generic

type AllHave c a = GFields c (Rep a)

type family GFields (c :: * -> Constraint) (f :: * -> *) :: Constraint
type instance GFields c (M1 i j f) = GFields c f
type instance GFields c (f :+: g)  = (GFields c f, GFields c g)
type instance GFields c (f :*: g)  = (GFields c f, GFields c g)
type instance GFields c U1         = ()
type instance GFields c (K1 i a)   = c a

deriving instance AllHave Show (Gql q m s ts) => Show (Gql q m s ts)
deriving instance AllHave Eq (Gql q m s ts) => Eq (Gql q m s ts)
instance ToJSON (q 'Response) => ToJSON (Gql q m s 'Response) where
  toJSON (Gql q) = object ["data" .= (toJSON q ^? key "query") ]


newtype NoNothingJSON a = NoNothingJSON a deriving Generic

instance (Generic a, ToJSON a, GToJSON Zero (Rep a)) => ToJSON (NoNothingJSON a) where
  toJSON (NoNothingJSON a)  = genericToJSON (defaultOptions { omitNothingFields = True }) a




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

