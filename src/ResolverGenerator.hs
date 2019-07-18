module ResolverGenerator where


import SchemaGenerator
import Args
import GHC.Generics
-- type IsResolvable record = ( Generic (record 'Data)
--                             , Generic (record 'Query)
--                             , Generic (record 'Response)
--                             , GHydrate (Rep (record 'Data))
--                                     (Rep (record 'Query))
--                                     (Rep (record 'Response))
--                             )

-- resolve :: record 'Resolver -> record 'Query -> record 'Response
-- resolve d query = undefined

-- class GResolve rv qu rp where
--     gResolve :: rv x -> qu x -> IO (rp x)

-- instance (GResolve rv qu rp) =>
--     GResolve (M1 x y rv)
--              (M1 x y qu)
--              (M1 x y rp) where
--     gResolve (M1 rv) (M1 qu) = M1 <$> (gResolve rv qu)

-- instance (GResolve frv fqu frp, GResolve grv gqu grp) =>
--     GResolve (frv :*: grv)
--              (fqu :*: gqu)
--              (frp :*: grp) where
--     gResolve (frv :*: grv) (fqu :*: gqu) = (:*:) <$> (gResolve frv fqu) <*> (gResolve grv gqu)

-- | Q, RV1, RP3
-- instance (Resolve rv qu rp) => 
--          GResolve (K1 x rv)
--                   (K1 x qu)
--                   (K1 x rp) where
--          gResolve (K1 rv) (K1 qu) = K1 <$> resolve rv qu

class Resolve rv qu rp where
    resolve :: rv -> qu -> rp


-- | Base Cases
instance Resolve (record 'Query -> IO (record 'Response))
                 (Maybe ((Args ('[]), record 'Query)))
                 (IO (Maybe (record 'Response))) where
        resolve (f) (Nothing) = pure Nothing
        resolve (f) (Just (ANil, query)) = Just <$> f query
        
instance Resolve (record 'Query -> IO ([record 'Response]))
                 (Maybe ((Args ('[]), record 'Query)))
                 (IO (Maybe ([record 'Response]))) where
        resolve (f) (Nothing) = pure Nothing
        resolve (f) (Just (ANil, query)) = Just <$> f query

instance Resolve (IO (scalar))
                 (Maybe ((Args ('[]), ())))
                 (IO (Maybe (scalar))) where
        resolve (f) (Nothing) = pure Nothing
        resolve (s) (Just (ANil, ())) = Just <$> s



-- | Args Case
instance Resolve (Arg n t -> rv)
                 (Maybe ((Args ('(n, t)':args), ru)))
                 (rp) where
        resolve (f) (Nothing) = resolve @rv @'(args, ru) @rp f Nothing
        resolve (f) (Just (ANil, query)) = Just <$> f query


-- instance Resolve (Arg n t -> rv)
--                  (Maybe ((Args ('(n, t)':args), qu)))
--                  (rp)
--         resolve (f) (Nothing) = resolve
         

-- $> :kind! Rep (User 'Resolver)
