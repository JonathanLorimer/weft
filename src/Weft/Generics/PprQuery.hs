{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}

module Weft.Generics.PprQuery
  ( HasPprQuery
  , pprQuery
  , pprArg
  , gPprQuery
  ) where

import           Data.Functor ((<&>))
import qualified Data.Map as M
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits hiding (ErrorMessage (..))
import           Prelude hiding ((<>))
import           Text.PrettyPrint.HughesPJ
import           Weft.Internal.ArgTypes
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
class GPprQuery (rq :: * -> *) where
  gPprQuery :: rq x -> Doc

instance (GPprQuery f, GPprQuery g) => GPprQuery (f :*: g) where
  gPprQuery (f :*: g) = vcat
    [ gPprQuery f
    , gPprQuery g
    ]
instance {-# OVERLAPPABLE #-} GPprQuery f => GPprQuery (M1 _1 _2 f) where
  gPprQuery (M1 f) = gPprQuery f

instance ( GPprQuery (M1 S ('MetaSel ('Just name) b c d) (K1 _2 (Magic 'Query t)))
         ) => GPprQuery (M1 S ('MetaSel ('Just name) b c d) (K1 _2 (ToMagic 'Query t))) where
    gPprQuery (M1 (K1 (ToMagic t))) = gPprQuery @(M1 S ('MetaSel ('Just name) b c d) (K1 _2 (Magic 'Query t))) $ M1 $ K1 t

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


pprAliasIfDifferent :: String -> Text -> Doc -> Doc
pprAliasIfDifferent name (T.unpack -> alias) doc
  | name == alias = doc
  | otherwise = sep
      [ text alias <> char ':'
      , doc
      ]


class PprEachArg (ts) where
  pprEachArg :: Args ts -> [Doc]

instance PprEachArg '[] where
  pprEachArg ANil = []

instance (IsArgType t, PprEachArg args) => PprEachArg ('(n, t) ': args) where
  pprEachArg (arg :@@ args) = pprArg arg : pprEachArg args


pprArgs :: PprEachArg ts => Args ts -> Doc
pprArgs args =
  let args_docs = pprEachArg args
   in case all isEmpty args_docs of
        True -> empty
        False -> parens $ sep $ punctuate (char ',') $ filter (not . isEmpty) args_docs

