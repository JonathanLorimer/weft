module Weft.Types where

import GHC.TypeLits
import Test.QuickCheck (Arbitrary (..))


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

  Magic 'Query    ts                 = (MagicQueryResult (UnravelArgs ts))

  Magic 'Response (Arg n t -> a)     = Magic 'Response a
  Magic 'Response [record 'Response] = Maybe [record 'Response]                   -- RP1
  Magic 'Response (record 'Response) = Maybe (record 'Response)                   -- RP2
  Magic 'Response scalar             = Maybe scalar                               -- RP3

  Magic 'Schema   ts                 = Field (Fst (UnravelArgs ts))


------------------------------------------------------------------------------
-- |
type family ConsFirst (a :: k1) (b :: ([k1], k2)) :: ([k1], k2) where
  ConsFirst a '(b, c) = '(a ': b, c)

type family UnravelArgs (t :: *) :: ([(Symbol, *)], *) where
  UnravelArgs (Arg t n -> a) = ConsFirst '(t, n) (UnravelArgs a)
  UnravelArgs a        = '( '[], a)

type family MagicQueryResult (u :: ([(Symbol, *)], *)) :: * where
  MagicQueryResult '(ts, [record 'Query]) = Maybe (Args ts, record 'Query)
  MagicQueryResult '(ts, record 'Query)   = Maybe (Args ts, record 'Query)
  MagicQueryResult '(ts, a)               = Maybe (Args ts, ())

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

