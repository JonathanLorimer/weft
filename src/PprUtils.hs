module PprUtils where

import Data.Proxy
import GHC.TypeLits
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ
import Weft.Types



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

