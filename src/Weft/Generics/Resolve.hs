module Weft.Generics.Resolve
  ( HasResolve
  , resolve
  ) where

import GHC.Generics
import GHC.TypeLits
import Weft.Types


------------------------------------------------------------------------------
-- |
type HasResolve record =
  ( GResolve (Rep (record 'Resolver))
             (Rep (record 'Query))
             (Rep (record 'Response))
  , Generic (record 'Resolver)
  , Generic (record 'Query)
  , Generic (record 'Response)
  )


------------------------------------------------------------------------------
-- |
resolve
    :: HasResolve record
    => record 'Resolver
    -> record 'Query
    -> IO (record 'Response)
resolve d query = to <$> gResolve (from d) (from query)


------------------------------------------------------------------------------
-- |
class GResolve rv qu rp where
  gResolve :: rv x -> qu x -> IO (rp x)

instance ( GResolve rv qu rp
         ) => GResolve (M1 x y rv)
                       (M1 x y qu)
                       (M1 x y rp) where
  gResolve (M1 rv) (M1 qu) = M1 <$> gResolve rv qu

instance ( GResolve frv fqu frp
         , GResolve grv gqu grp
         ) => GResolve (frv :*: grv)
                       (fqu :*: gqu)
                       (frp :*: grp) where
  gResolve (frv :*: grv) (fqu :*: gqu) =
    (:*:) <$> gResolve frv fqu
          <*> gResolve grv gqu

-- | Q, RV1, RP3
instance (ResolveField rv qu (IO rp)) =>
         GResolve (K1 x rv)
                  (K1 x qu)
                  (K1 x rp) where
  gResolve (K1 rv) (K1 qu) = K1 <$> resolveField rv qu


------------------------------------------------------------------------------
-- |
class ResolveField rv qu rp where
  resolveField :: rv -> qu -> rp


-- | Base Cases
instance ResolveField (record 'Query -> IO (record 'Response))
                      (Maybe ((Args ('[]), record 'Query)))
                      (IO (Maybe (record 'Response))) where
  resolveField _ Nothing = pure Nothing
  resolveField f (Just (ANil, query)) = Just <$> f query

instance ResolveField (record 'Query -> IO ([record 'Response]))
                      (Maybe ((Args ('[]), record 'Query)))
                      (IO (Maybe ([record 'Response]))) where
  resolveField _ Nothing = pure Nothing
  resolveField f (Just (ANil, query)) = Just <$> f query

instance ResolveField (IO (scalar))
                      (Maybe ((Args ('[]), ())))
                      (IO (Maybe (scalar))) where
  resolveField _ (Nothing) = pure Nothing
  resolveField s (Just (ANil, ())) = Just <$> s


-- | Args Case
instance {-# OVERLAPPING #-}
         ( KnownSymbol n
         , ResolveField rv
                        (Maybe (Args args, ru))
                        rp
         ) => ResolveField (Arg n (Maybe t) -> rv)
                           (Maybe ( Args ('(n, Maybe t) ': args)
                                  , ru
                                  ))
                           rp where
  resolveField f Nothing =
    resolveField @rv
                 @(Maybe (Args args, ru))
                 @rp
                 (f $ Arg Nothing)
                 Nothing
  resolveField f (Just (arg :@@ args, query)) =
    resolveField @rv
                 @(Maybe (Args args, ru))
                 @rp
                 (f arg)
                 (Just (args, query))

instance ( ResolveField rv
                        (Maybe (Args args, ru))
                        rp
         ) => ResolveField (Arg n t -> rv)
                           (Maybe ( Args ('(n, t) ': args)
                                  , ru
                                  ))
                           rp where
  resolveField _ Nothing = error "impossible"
  resolveField f (Just (arg :@@ args, query)) =
    resolveField @rv
                 @(Maybe (Args args, ru))
                 @rp
                 (f arg)
                 (Just (args, query))

