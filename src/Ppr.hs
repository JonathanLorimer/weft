module Ppr where

import Data.Proxy
import GHC.TypeLits
import TestData
import Text.PrettyPrint.HughesPJ
import SchemaGenerator
import GHC.Generics
import Prelude hiding ((<>))


pprBang :: Bool -> Doc
pprBang False = empty
pprBang True = char '!'


pprType :: GqlType -> Doc
pprType (GqlSingle b t) = text t <> pprBang b
pprType (GqlList b t) = brackets (pprType t) <> pprBang b


pprArg :: NameType -> Doc
pprArg (NameType n t) =
  sep [ text n <> char ':'
      , pprType t
      ]

pprField :: Field args -> Doc
pprField (Field (NameType n t) []) =
  sep [ mconcat
        [ text n
        , char ':'
        ]
      , pprType t
      ]
pprField (Field (NameType n t) args) =
  sep [ mconcat
        [ text n
        , parens (sep $ punctuate (char ',') $ fmap pprArg args)
        , char ':'
        ]
      , pprType t
      ]

pprRecord
    :: (GPprRecord (Rep (record 'Schema)), Generic (record 'Schema))
    => record 'Schema
    -> Doc
pprRecord = gPprRecord . from


class GPprRecord rs where
  gPprRecord :: rs x -> Doc

instance (KnownSymbol name, GPprRecord f) => GPprRecord (M1 D ('MetaData name _1 _2 _3) f) where
  gPprRecord (M1 f) = vcat
    [ hsep
      [ text "type"
      , text $ symbolVal $ Proxy @name
      , char '{'
      ]
    , nest 4 $ gPprRecord f
    , char '}'
    ]

instance (GPprRecord f, GPprRecord g) => GPprRecord (f :*: g) where
  gPprRecord (f :*: g) = vcat
    [ gPprRecord f
    , gPprRecord g
    ]

instance {-# OVERLAPPABLE #-} GPprRecord f => GPprRecord (M1 _1 _2 f) where
  gPprRecord (M1 f) = gPprRecord f

instance GPprRecord (K1 _1 (Field args)) where
  gPprRecord (K1 f) = pprField f

