module Ppr where

import Args
import Parser
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

class PprEachArg (ts) where
  pprEachArg :: Args ts -> [Doc]

instance PprEachArg '[] where
  pprEachArg ANil = []

instance (PprArg t, PprEachArg args) => PprEachArg ('(n, t) ': args) where
  pprEachArg (arg :@@ args) = pprArg arg : pprEachArg args


pprArgs :: PprEachArg ts => Args ts -> Doc
pprArgs args =
  let args_docs = pprEachArg args
   in case all isEmpty args_docs of
        True -> empty
        False -> parens $ sep $ punctuate (char ',') args_docs

class PprArg t where
  pprArg :: Arg n t -> Doc

instance Show t => PprArg t where
  pprArg (Arg v :: Arg n t) =
    sep [ text (symbolVal $ Proxy @n) <> char ':'
        , text $ show v
        ]

instance {-# OVERLAPPING #-} Show t => PprArg (Maybe t) where
  pprArg (Arg Nothing) = empty
  pprArg (Arg (Just v) :: Arg n (Maybe t)) =
    sep [ text (symbolVal $ Proxy @n) <> char ':'
        , text $ show v
        ]

pprFieldArg :: NameType -> Doc
pprFieldArg (NameType n t) =
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
        , parens (sep $ punctuate (char ',') $ fmap pprFieldArg args)
        , char ':'
        ]
      , pprType t
      ]

type HasPprRecord record = (GPprSchema (Rep (record 'Schema)), Generic (record 'Schema))

pprRecord
    :: HasPprRecord record
    => record 'Schema
    -> Doc
pprRecord = gPprSchema . from

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


class GPprSchema rs where
  gPprSchema :: rs x -> Doc

instance (KnownSymbol name, GPprSchema f) => GPprSchema (M1 D ('MetaData name _1 _2 _3) f) where
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


class GPprQuery rq where
  gPprQuery :: rq x -> Doc

type HasPprQuery record = (Generic (record 'Query), GPprQuery (Rep (record 'Query)))

pprQuery :: HasPprQuery record => record 'Query -> Doc
pprQuery q = sep
  [ char '{'
  , nest 4 $ gPprQuery $ from q
  , char '}'
  ]


instance (GPprQuery f, GPprQuery g) => GPprQuery (f :*: g) where
  gPprQuery (f :*: g) = vcat
    [ gPprQuery f
    , gPprQuery g
    ]

instance {-# OVERLAPPABLE #-} GPprQuery f => GPprQuery (M1 _1 _2 f) where
  gPprQuery (M1 f) = gPprQuery f

instance (KnownSymbol name, PprEachArg args, HasPprQuery record) => GPprQuery (M1 S ('MetaSel ('Just name) b c d) (K1 x (Maybe (Args args, record 'Query)))) where
  gPprQuery (M1 (K1 Nothing)) = empty
  gPprQuery (M1 (K1 (Just (args, rec)))) =
    sep
      [ text (symbolVal $ Proxy @name) <> pprArgs args
      , pprQuery rec
      ]

instance (KnownSymbol name, PprEachArg args) => GPprQuery (M1 S ('MetaSel ('Just name) b c d) (K1 x (Maybe (Args args, ())))) where
  gPprQuery (M1 (K1 Nothing)) = empty
  gPprQuery (M1 (K1 (Just (args, rec)))) =
    mconcat
      [ text $ symbolVal $ Proxy @name
      , pprArgs args
      ]


type HasFindTypes record =
  ( GFindTypes (Rep (record 'Data))
  , GHasSchema (Rep (record 'Data)) (Rep (record 'Schema))
  , GPprSchema (Rep (record 'Schema))
  , Generic (record 'Data)
  , Generic (record 'Schema)
  , Typeable record
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

