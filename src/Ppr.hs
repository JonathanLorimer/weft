module Ppr where

import           Data.List.NonEmpty
import qualified Data.Map as M
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits
import           Prelude hiding ((<>))
import           SchemaGenerator
import           TestData
import           Text.PrettyPrint.HughesPJ



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


class GPprRecord rs where
  gPprRecord :: rs x -> Doc

instance (KnownSymbol name, GPprRecord f) => GPprRecord (M1 D ('MetaData name _1 _2 _3) f) where
  gPprRecord (M1 f) = pprTypeHerald (symbolVal $ Proxy @name) $ gPprRecord f

instance (GPprRecord f, GPprRecord g) => GPprRecord (f :*: g) where
  gPprRecord (f :*: g) = vcat
    [ gPprRecord f
    , gPprRecord g
    ]

instance {-# OVERLAPPABLE #-} GPprRecord f => GPprRecord (M1 _1 _2 f) where
  gPprRecord (M1 f) = gPprRecord f

instance GPprRecord (K1 _1 (Field args)) where
  gPprRecord (K1 f) = pprField f

type HasFindTypes record =
  ( GFindTypes (Rep (record 'Data))
  , GHasSchema (Rep (record 'Data)) (Rep (record 'Schema))
  , GPprRecord (Rep (record 'Schema))
  , Typeable record
  , Generic (record 'Data)
  , Generic (record 'Schema)
  )

allTypes :: forall record. HasFindTypes record => [Doc]
allTypes = fmap snd $ M.toList $ findTypes @record M.empty

findTypes
    :: forall record
     . HasFindTypes record
    => M.Map TypeRep Doc
    -> M.Map TypeRep Doc
findTypes m =
  case M.lookup name m of
    Just x -> m
    Nothing -> gFindTypes @(Rep (record 'Data)) $ M.insert name (pprRecord $ schema @record) m
  where
    name = typeRep $ Proxy @record


class GFindTypes rd where
  gFindTypes :: M.Map TypeRep Doc -> M.Map TypeRep Doc

instance GFindTypes rd => GFindTypes (M1 _1 _2 rd) where
  gFindTypes = gFindTypes @rd

instance (GFindTypes rd, GFindTypes rd') => GFindTypes (rd :*: rd') where
  gFindTypes m =
    let m' = gFindTypes @rd m
     in gFindTypes @rd' m'

instance (HasFindTypes record) => GFindTypes (K1 _1 (record 'Data)) where
  gFindTypes = findTypes @record

instance (HasFindTypes record) => GFindTypes (K1 _1 [record 'Data]) where
  gFindTypes = findTypes @record

instance (HasFindTypes record) => GFindTypes (K1 _1 (NonEmpty (record 'Data))) where
  gFindTypes = findTypes @record

instance {-# OVERLAPPABLE #-} GFindTypes (K1 _1 _2) where
  gFindTypes = id

