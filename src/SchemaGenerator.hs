module SchemaGenerator where

import GHC.Generics
import GHC.TypeLits
import Data.Proxy
import Data.Typeable

newtype Id = Id String deriving (Generic, Show)
newtype Name = Name String deriving (Generic, Show)



-- | Test Data
data User' ts = User { userId       :: Magic ts Id 
                     , userName     :: Magic ts Name 
                     , userFriend   :: Magic ts [User' ts]} deriving (Generic)
deriving instance Show (User' 'Schema)
deriving instance Show (User' 'Response)

type User = User' 'Data
type UserQuery = User' 'Query

user3 :: User
user3 = User (Id "3") (Name "Claire") [user1, user2]

user2 :: User
user2 = User (Id "2") (Name "Sandy") [user1, user3]

user1 :: User
user1 = User (Id "1") (Name "Jonathan") []

userQ :: User' 'Query
userQ = User True True $ Just $ User False True $ Just (User True True Nothing)


-- | Library Code
data TypeState = Query | Data | Schema | Response

data SchemaRecord = SchemaRecord { fieldName :: String
                                 , fieldType :: String } deriving (Show)

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

type IsHydrateable record = ( Generic (record 'Data)
                            , Generic (record 'Query)
                            , Generic (record 'Response)
                            , GHydrate (Rep (record 'Data))
                                    (Rep (record 'Query))
                                    (Rep (record 'Response))
                            )

hydrate :: IsHydrateable record => record 'Data -> record 'Query -> record 'Response
hydrate d query = to $ gHydrate (from d) (from query)

class GHydrate fd fq fr where
    gHydrate :: fd x -> fq x -> fr x

instance (GHydrate fd fq fr) => 
    GHydrate (M1 x y fd)
             (M1 x y fq)
             (M1 x y fr) where
    gHydrate (M1 fd) (M1 fq) = M1 $ (gHydrate fd fq)

instance (GHydrate fd fq fr, GHydrate gd gq gr) => 
    GHydrate (fd :*: gd)
             (fq :*: gq)
             (fr :*: gr) where 
    gHydrate (fd :*: gd) (fq :*: gq) = (gHydrate fd fq) :*: (gHydrate gd gq) 

-- Q3, R3
instance GHydrate (K1 x (d))
                  (K1 x (Bool))
                  (K1 x (Maybe d)) where 
    gHydrate (K1 _) (K1 False) = K1 Nothing 
    gHydrate (K1 d) (K1 True)  = K1 $ Just d

-- Q2, R2
instance IsHydrateable record =>
         GHydrate (K1 x (record 'Data))
                  (K1 x (Maybe (record 'Query)))
                  (K1 x (Maybe (record 'Response))) where 
    gHydrate (K1 _) (K1 Nothing) = K1 Nothing 
    gHydrate (K1 d) (K1 (Just q)) = K1 $ Just $ hydrate d q

-- Q1, R1 List
instance IsHydrateable record =>
         GHydrate (K1 x [record 'Data])
                  (K1 x (Maybe (record 'Query)))
                  (K1 x (Maybe [record 'Response])) where 
    gHydrate (K1 _) (K1 Nothing) = K1 Nothing 
    gHydrate (K1 d) (K1 (Just q)) = K1 $ Just $ (flip hydrate $ q) <$> d


-- $> hydrate user2 userQ

-- Id -> Name -> [User' 'Data] -> User' 'Data
-- Bool -> Bool -> Maybe (User' 'Query) -> User' 'Query
-- Maybe Id     -> Maybe Name -> Maybe [User' 'Response] -> User' 'Response