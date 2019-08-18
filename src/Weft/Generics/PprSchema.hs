module Weft.Generics.PprSchema
  ( HasMagicPprSchema
  , magicPprSchema
  ) where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ
import Weft.PprUtils
import Weft.Internal.Types


------------------------------------------------------------------------------
-- |
type HasMagicPprSchema record =
  ( GPprSchema (J record 'Schema)
  , Generic record
  )


------------------------------------------------------------------------------
-- |
magicPprSchema
    :: HasMagicPprSchema record
    => JHKD record 'Schema
    -> Doc
magicPprSchema = gPprSchema . runHKD


------------------------------------------------------------------------------
-- |
class GPprSchema (rs :: * -> *) where
  gPprSchema :: rs x -> Doc

instance ( KnownSymbol name
         , GPprSchema f
         ) => GPprSchema (M1 D ('MetaData name _1 _2 _3) f) where
  gPprSchema (M1 f) = pprTypeHerald (symbolVal $ Proxy @name) $ gPprSchema f

instance (GPprSchema f, GPprSchema g) => GPprSchema (f :*: g) where
  gPprSchema (f :*: g) = vcat
    [ gPprSchema f
    , gPprSchema g
    ]

instance {-# OVERLAPPABLE #-} GPprSchema f => GPprSchema (M1 _1 _2 f) where
  gPprSchema (M1 f) = gPprSchema f

instance GPprSchema (K1 _1 (Field args)) where
  gPprSchema (K1 f) = pprField f

instance GPprSchema (K1 _1 (Magic 'Schema t)) => GPprSchema (K1 _1 (ToMagic 'Schema t)) where
  gPprSchema (K1 (ToMagic f)) = gPprSchema @(K1 _1 (Magic 'Schema t)) $ K1 f


------------------------------------------------------------------------------
-- |
pprTypeHerald :: String -> Doc -> Doc
pprTypeHerald name doc = vcat
  [ hsep
    [ text "type"
    , text name
    , char '{'
    ]
  , nest 4 doc
  , char '}'
  ]

