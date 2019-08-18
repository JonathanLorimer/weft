module Weft.Generics.Resolve
  ( HasResolve
  , resolve
  , HasMagicResolve
  , magicResolve
  ) where

import qualified Data.Map as M
import           Data.Text (Text)
import           GHC.Generics
import           Weft.Internal.Types hiding (query)


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
type HasMagicResolve record =
  ( GResolve (J record 'Resolver)
             (J record 'Query)
             (J record 'Response)
  , Generic record
  )


------------------------------------------------------------------------------
-- |
resolve
    :: HasResolve record
    => record 'Resolver
    -> record 'Query
    -> IO (record 'Response)
resolve rv query = to <$> gResolve (from rv) (from query)


------------------------------------------------------------------------------
-- |
magicResolve
    :: HasMagicResolve record
    => JHKD record 'Resolver
    -> JHKD record 'Query
    -> IO (JHKD record 'Response)
magicResolve rv query = HKD <$> gResolve (runHKD rv) (runHKD query)


------------------------------------------------------------------------------
-- |
class GResolve (rv :: * -> *) (qu :: * -> *) (rp :: * -> *) where
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
instance ResolveField (M1 _1 _2 q _3 -> IO (M1 _1 _2 rp _3))
                      (Args '[], M1 _1 _2 q _3)
                      (M1 _1 _2 rp _3) where
  resolveField f (ANil, query) = f query

instance ResolveField (M1 _1 _2 q _3 -> IO [M1 _1 _2 rp _3])
                      (Args '[], M1 _1 _2 q _3)
                      [M1 _1 _2 rp _3] where
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


-- | Q, RV1, RP3
instance GResolve (K1 x (Magic 'Resolver r))
                  (K1 x (Magic 'Query r))
                  (K1 x (Magic 'Response r)) =>
      GResolve (K1 x (ToMagic 'Resolver r))
               (K1 x (ToMagic 'Query r))
               (K1 x (ToMagic 'Response r)) where
  gResolve (K1 (ToMagic rv)) (K1 (ToMagic qu)) =
    K1 . ToMagic . unK1 @x <$>
      gResolve (K1 @x rv) (K1 @x qu)

