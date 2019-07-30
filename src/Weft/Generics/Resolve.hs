module Weft.Generics.Resolve
  ( HasResolve
  , resolve
  ) where

import GHC.Generics
import GHC.TypeLits
import Weft.Internal.Types


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
instance (ResolveField rv qu rp) =>
         GResolve (K1 x rv)
                  (K1 x (Maybe qu))
                  (K1 x (Maybe rp)) where
  gResolve _ (K1 Nothing) = pure $ K1 Nothing
  gResolve (K1 rv) (K1 (Just qu)) = K1 . Just <$> resolveField rv qu


------------------------------------------------------------------------------
-- |
class ResolveField rv qu rp where
  resolveField :: rv -> qu -> IO rp


-- | Base Cases
instance ResolveField (record 'Query -> IO (record 'Response))
                      (Args '[], record 'Query)
                      (record 'Response) where
  resolveField f (ANil, query) = f query

instance ResolveField (record 'Query -> IO ([record 'Response]))
                      (Args '[], record 'Query)
                      [record 'Response] where
  resolveField f (ANil, query) = f query

instance ResolveField (IO scalar)
                      (Args '[], ())
                      scalar where
  resolveField s (ANil, ()) = s


-- | Args Case
instance {-# OVERLAPPING #-}
         ( KnownSymbol n
         , ResolveField rv (Args args, ru) rp
         ) => ResolveField (Arg n (Maybe t) -> rv)
                           (Args ('(n, Maybe t) ': args), ru)
                           rp where
  resolveField f (arg :@@ args, query) =
    resolveField (f arg) (args, query)

instance ResolveField rv (Args args, ru) rp
      => ResolveField (Arg n t -> rv)
                      (Args ('(n, t) ': args), ru)
                      rp where
  resolveField f (arg :@@ args, query) =
    resolveField (f arg) (args, query)

