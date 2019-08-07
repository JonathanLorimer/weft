{-# LANGUAGE ViewPatterns #-}

module Weft.Generics.PprQuery
  ( HasPprQuery
  , pprQuery
  , pprArg
  ) where

import           Data.Char
import           Data.Functor ((<&>))
import qualified Data.Map as M
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits hiding (ErrorMessage (..))
import           Prelude hiding ((<>))
import           Text.PrettyPrint.HughesPJ
import           Weft.Internal.Types


------------------------------------------------------------------------------
-- |
type HasPprQuery record =
  ( Generic (record 'Query)
  , GPprQuery (Rep (record 'Query))
  )


------------------------------------------------------------------------------
-- |
pprQuery :: HasPprQuery record => record 'Query -> Doc
pprQuery q = gPprQuery $ from q


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
                              (K1 x (M.Map Text (Args args, record 'Query)))) where
  gPprQuery (M1 (K1 m)) = vcat $ M.toList m <&> \(alias, (args, rec)) ->
    pprAliasIfDifferent name alias $
      sep
        [ text name <> pprArgs args
        , char '{'
        , nest 4 $ pprQuery rec
        , char '}'
        ]
    where
      name = symbolVal $ Proxy @name

instance ( KnownSymbol name
         , PprEachArg args
         ) => GPprQuery (M1 S ('MetaSel ('Just name) b c d)
                              (K1 x (M.Map Text (Args args, ())))) where
  gPprQuery (M1 (K1 m)) = vcat $ M.toList m <&> \(alias, (args, ())) ->
    pprAliasIfDifferent name alias $
      mconcat
        [ text name
        , pprArgs args
        ]
    where
      name = symbolVal $ Proxy @name

class GPprThing rq where
  gPprThing :: rq x -> Doc

instance  GPprThing f => GPprThing (M1 _1 _2 f) where
   gPprThing (M1 f) = gPprThing f

instance  GPprInput (f :*: g) => GPprThing (f :*: g) where
   gPprThing z = vcat
    [ char '{'
    , nest 4 $ gPprInput z
    , char '}'
    ]

instance  GPprEnum (f :+: g) => GPprThing (f :+: g) where
   gPprThing = gPprEnum

class GPprEnum rq where
  gPprEnum :: rq x -> Doc

instance (GPprEnum f, GPprEnum g) => GPprEnum (f :+: g) where
  gPprEnum (L1 f) = gPprEnum f
  gPprEnum (R1 g) = gPprEnum g

instance ( KnownSymbol name
         ) => GPprEnum (M1 C ('MetaCons name _1 _2) U1) where
  gPprEnum (M1 U1) = text $ fmap toUpper $ symbolVal $ Proxy @name

class GPprInput rq where
  gPprInput :: rq x -> Doc

instance (GPprInput f, GPprInput g) => GPprInput (f :*: g) where
  gPprInput (f :*: g) = vcat
    [ gPprInput f <> char ','
    , gPprInput g
    ]

instance ( KnownSymbol name
         , PprArg t
         ) => GPprInput (M1 S ('MetaSel ('Just name) b c d)
                              (K1 x t)) where
  gPprInput (M1 (K1 t)) = pprArg $ Arg @name t


pprAliasIfDifferent :: String -> Text -> Doc -> Doc
pprAliasIfDifferent name (T.unpack -> alias) doc
  | name == alias = doc
  | otherwise = sep
      [ text alias <> char ':'
      , doc
      ]


class PprArg t where
  pprArg :: Arg n t -> Doc


pprArgWithName :: forall n. KnownSymbol n => String -> Doc
pprArgWithName val =
    sep [ text (symbolVal $ Proxy @n) <> char ':'
        , text val
        ]

instance PprArg Integer where
  pprArg (Arg v :: Arg n Integer) =
    pprArgWithName @n $ show v

instance PprArg Int where
  pprArg (Arg v :: Arg n Int) =
    pprArgWithName @n $ show v

instance PprArg Double where
  pprArg (Arg v :: Arg n Double) =
    pprArgWithName @n $ show v

instance PprArg Bool where
  pprArg (Arg v :: Arg n Bool) =
    pprArgWithName @n $ fmap toLower $ show v

instance PprArg String where
  pprArg (Arg v :: Arg n String) =
    pprArgWithName @n $ show v

instance PprArg ID where
  pprArg (Arg (ID v) :: Arg n ID) =
    pprArgWithName @n $ show v

instance {-# OVERLAPPABLE #-} (Generic t, GPprThing (Rep t)) => PprArg t where
  pprArg (Arg v :: Arg n t) =
    sep [ text (symbolVal $ Proxy @n) <> char ':'
        , gPprThing $ from v
        ]

instance {-# OVERLAPPING #-} PprArg t => PprArg (Maybe t) where
  pprArg (Arg Nothing) = empty
  pprArg (Arg (Just v) :: Arg n (Maybe t)) = pprArg (Arg @n v)


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

