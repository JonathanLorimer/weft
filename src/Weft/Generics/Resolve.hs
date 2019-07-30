module Weft.Generics.Resolve
  ( HasResolve
  , resolve
  ) where

import qualified Data.Map as M
import           Data.Text (Text)
import           GHC.Generics
import           Weft.Internal.Types


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
                  (K1 x (M.Map Text qu))
                  (K1 x (M.Map Text rp)) where
  gResolve (K1 rv) (K1 qu) = K1 <$> traverse (resolveField rv) qu


------------------------------------------------------------------------------
-- |
class ResolveField rv qu rp where
  resolveField :: rv -> qu -> IO rp


-- | Base Cases
instance ResolveField (record 'Query -> IO (record 'Response))
                      (Args '[], record 'Query)
                      (record 'Response) where
  resolveField f (ANil, query) = f query

instance ResolveField (record 'Query -> IO [record 'Response])
                      (Args '[], record 'Query)
                      [record 'Response] where
  resolveField f (ANil, query) = f query

instance ResolveField (IO scalar)
                      (Args '[], ())
                      scalar where
  resolveField s (ANil, ()) = s


instance ResolveField rv (Args args, ru) rp
      => ResolveField (Arg n t -> rv)
                      (Args ('(n, t) ': args), ru)
                      rp where
  resolveField f (arg :@@ args, query) =
    resolveField (f arg) (args, query)

