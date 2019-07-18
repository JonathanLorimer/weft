module Weft.Generics.PprQuery
  ( HasPprQuery
  , pprQuery
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
type HasPprQuery record =
  ( Generic (record 'Query)
  , GPprQuery (Rep (record 'Query))
  )


------------------------------------------------------------------------------
-- |
pprQuery :: HasPprQuery record => record 'Query -> Doc
pprQuery q = sep
  [ char '{'
  , nest 4 $ gPprQuery $ from q
  , char '}'
  ]


------------------------------------------------------------------------------
-- |
class GPprQuery rq where
  gPprQuery :: rq x -> Doc

instance (GPprQuery f, GPprQuery g) => GPprQuery (f :*: g) where
  gPprQuery (f :*: g) = vcat
    [ gPprQuery f
    , gPprQuery g
    ]
instance {-# OVERLAPPABLE #-} GPprQuery f => GPprQuery (M1 _1 _2 f) where
  gPprQuery (M1 f) = gPprQuery f

instance ( KnownSymbol name
         , PprEachArg args
         , HasPprQuery record
         ) => GPprQuery (M1 S ('MetaSel ('Just name) b c d)
                              (K1 x (Maybe (Args args, record 'Query)))) where
  gPprQuery (M1 (K1 Nothing)) = empty
  gPprQuery (M1 (K1 (Just (args, rec)))) =
    sep
      [ text (symbolVal $ Proxy @name) <> pprArgs args
      , pprQuery rec
      ]

instance ( KnownSymbol name
         , PprEachArg args
         ) => GPprQuery (M1 S ('MetaSel ('Just name) b c d)
                              (K1 x (Maybe (Args args, ())))) where
  gPprQuery (M1 (K1 Nothing)) = empty
  gPprQuery (M1 (K1 (Just (args, ())))) =
    mconcat
      [ text $ symbolVal $ Proxy @name
      , pprArgs args
      ]

