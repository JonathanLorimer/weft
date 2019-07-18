module Weft.Generics.PprSchema
  ( HasPprSchema
  , pprSchema
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
type HasPprSchema record =
  ( GPprSchema (Rep (record 'Schema))
  , Generic (record 'Schema)
  )


------------------------------------------------------------------------------
-- |
pprSchema
    :: HasPprSchema record
    => record 'Schema
    -> Doc
pprSchema = gPprSchema . from


------------------------------------------------------------------------------
-- |
class GPprSchema rs where
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

