{-# LANGUAGE ViewPatterns #-}

module Weft.Generics.PprQuery
  ( HasMagicPprQuery
  , magicPprQuery
  , pprArg
  ) where

import           Data.Functor ((<&>))
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Prelude hiding ((<>))
import           Text.PrettyPrint.HughesPJ
import           Weft.Internal.ArgTypes
import           Weft.Internal.Types
import Weft.Internal.GenericUtils


type HasMagicPprQuery record =
  ( Generic record
  , FoldP1 GPprTerm (J record 'Query)
  )

------------------------------------------------------------------------------
-- |
magicPprQuery :: HasMagicPprQuery record => JHKD record 'Query -> Doc
magicPprQuery = gPprQuery . runHKD


gPprQuery :: FoldP1 GPprTerm rep => rep x -> Doc
gPprQuery = vcat . foldP1 @GPprTerm ((pure .) . gPprTerm)

class GPprTerm (t :: *) where
  gPprTerm :: String -> t -> Doc

instance PprEachArg args => GPprTerm (M.Map Text (Args args, ())) where
  gPprTerm name m =
    vcat $ M.toList m <&> \(alias, (args, ())) ->
      pprAliasIfDifferent name alias $
        mconcat
          [ text name
          , pprArgs args
          ]

instance GPprTerm (Magic 'Query rec) => GPprTerm (ToMagic 'Query rec) where
  gPprTerm name = gPprTerm @(Magic 'Query rec) name . unMagic

instance ( FoldP1 GPprTerm (M1 _1 _2 _3)
         , PprEachArg args
         ) => GPprTerm (M.Map Text (Args args, M1 _1 _2 _3 _4)) where
  gPprTerm name m =
    vcat $ M.toList m <&> \(alias, (args, rec)) ->
      pprAliasIfDifferent name alias $
        sep
          [ text name <> pprArgs args
          , char '{'
          , nest 4 $ gPprQuery rec
          , char '}'
          ]


pprAliasIfDifferent :: String -> Text -> Doc -> Doc
pprAliasIfDifferent name (T.unpack -> alias) doc
  | name == alias = doc
  | otherwise = sep
      [ text alias <> char ':'
      , doc
      ]


class PprEachArg ts where
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

