module ResolverGenerator where

import TestData

import SchemaGenerator
import Args
import GHC.Generics
import Data.Typeable


testResolve :: User 'Resolver -> User 'Query -> IO (User 'Response)
testResolve = doResolve


doResolve
    :: ( GResolve (Rep (record 'Resolver)) (Rep (record 'Query)) (Rep (record 'Response))
       , Generic (record 'Resolver)
       , Generic (record 'Query)
       , Generic (record 'Response)
       )
    => record 'Resolver
    -> record 'Query
    -> IO (record 'Response)
doResolve d query = to <$> gResolve (from d) (from query)

class GResolve rv qu rp where
    gResolve :: rv x -> qu x -> IO (rp x)

instance (GResolve rv qu rp) =>
    GResolve (M1 x y rv)
             (M1 x y qu)
             (M1 x y rp) where
    gResolve (M1 rv) (M1 qu) = M1 <$> (gResolve rv qu)

instance (GResolve frv fqu frp, GResolve grv gqu grp) =>
    GResolve (frv :*: grv)
             (fqu :*: gqu)
             (frp :*: grp) where
    gResolve (frv :*: grv) (fqu :*: gqu) = (:*:) <$> (gResolve frv fqu) <*> (gResolve grv gqu)

-- | Q, RV1, RP3
instance (Resolve rv qu (IO rp)) =>
         GResolve (K1 x rv)
                  (K1 x qu)
                  (K1 x rp) where
         gResolve (K1 rv) (K1 qu) = K1 <$> resolve rv qu

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
instance {-# OVERLAPPING #-}
          (Resolve rv (Maybe (Args args, ru)) rp) =>
          Resolve (Arg n (Maybe t) -> rv)
                 (Maybe ((Args ('(n, Maybe t)':args), ru)))
                 (rp) where
        resolve f Nothing =
           resolve @rv @(Maybe (Args args, ru)) @rp (f $ Arg Nothing) Nothing
        resolve f (Just (arg :@@ args, query)) = resolve @rv @(Maybe (Args args, ru)) @rp (f arg) (Just (args, query))

instance (Resolve rv (Maybe (Args args, ru)) rp) =>
          Resolve (Arg n t -> rv)
                 (Maybe ((Args ('(n, t)':args), ru)))
                 (rp) where
        resolve f Nothing = error "impossible"
        resolve f (Just (arg :@@ args, query)) = resolve @rv @(Maybe (Args args, ru)) @rp (f arg) (Just (args, query))


-- instance Resolve (Arg n t -> rv)
--                  (Maybe ((Args ('(n, t)':args), qu)))
--                  (rp)
--         resolve (f) (Nothing) = resolve


-- $> :kind! Rep (User 'Resolver)
