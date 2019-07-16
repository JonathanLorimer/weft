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
                     , userFriends  :: Magic ts [User' ts]} deriving (Generic)
deriving instance Show (User' 'Schema)

type User = User' 'Data

user1 :: User' Data
user1 = User (Id "1") (Name "Jonathan") undefined


-- | Library Code
data TypeState = Query | Data | Schema | Response

data SchemaRecord = SchemaRecord { fieldName :: String
                                 , fieldType :: String } deriving (Show)

type family Magic (ts :: TypeState) a where
    Magic 'Data a = a
    Magic 'Query (f (record 'Query)) = Maybe (record 'Query)
    Magic 'Query (record 'Query) = Maybe (record 'Query)
    Magic 'Query field = Bool
    Magic 'Response (f (record 'Response)) = Maybe (f (record 'Response))
    Magic 'Response (record 'Response) = Maybe (record 'Response)
    Magic 'Response field = Maybe field
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


-- $> :t User @'Response

-- Id -> Name -> [User' 'Data] -> User' 'Data
-- Bool -> Bool -> Maybe (User' 'Query) -> User' 'Query
-- Maybe Id     -> Maybe Name -> Maybe [User' 'Response] -> User' 'Response