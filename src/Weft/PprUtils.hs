module Weft.PprUtils where

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ
import Weft.Internal.Types



pprBang :: Bool -> Doc
pprBang False = empty
pprBang True = char '!'


pprType :: GqlType -> Doc
pprType (GqlSingle b t) = text t <> pprBang b
pprType (GqlList b t) = brackets (pprType t) <> pprBang b

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

