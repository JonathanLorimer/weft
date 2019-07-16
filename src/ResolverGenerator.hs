module ResolverGenerator where

import TestData
import SchemaGenerator
import Data.Proxy
import Data.Typeable
import GHC.TypeLitsxw

data ResolverMeta = ResolverMeta { resolverInput :: [SchemaRecord]
                                 , resolverType  :: String
                                 } deriving (Show)

newtype Arg (name :: Symbol) t = Arg t

class HasResolverMeta t where
    resolverMeta :: ResolverMeta

instance (Typeable record) => 
    HasResolverMeta (record 'Query -> IO (record 'Response)) where
    resolverMeta = ResolverMeta [] $ show $ typeRep $ Proxy @record

-- getGod :: Arg "god" (God 'Data) -> God 'Query -> IO (God 'Response)

instance (Typeable t, KnownSymbol name, HasResolverMeta f) => HasResolverMeta (Arg name t -> f) where
    resolverMeta = rm {resolverInput = sr : (resolverInput rm)}
        where 
            rm = resolverMeta @f
            sr = SchemaRecord (symbolVal $ Proxy @name) (show $ typeRep $ Proxy @t)

-- $> resolverMeta @(Arg "id" Integer -> Arg "name" String -> User' 'Query -> IO (User' 'Response))

