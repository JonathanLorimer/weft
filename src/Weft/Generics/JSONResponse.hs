module Weft.Generics.JSONResponse
  ( HasJSONResponse
  , jsonResponse
  , combine
  ) where

import           Weft.Internal.Types
import           Weft.Internal.GenericUtils
import           GHC.Generics
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as M


------------------------------------------------------------------------------
-- |
type HasJSONResponse record =
  ( Generic record
  , FoldP1 GJsonTerm (J record 'Response)
  )


jsonResponse :: HasJSONResponse record => JHKD record 'Response -> Value
jsonResponse = gJsonResponse . runHKD


gJsonResponse :: FoldP1 GJsonTerm rep => rep x -> Value
gJsonResponse = foldr combine (object []) . foldP1 @GJsonTerm (const $ pure @[] . gJsonTerm)

combine :: Value -> Value -> Value
combine (Object a) (Object b) = Object $ a <> b
combine _ _                   = error "combine failed in JSONResponse, this should not have happened"

class GJsonTerm (t :: *) where
  gJsonTerm :: t -> Value

instance GJsonTerm (Magic ts t) => GJsonTerm (ToMagic ts t) where
  gJsonTerm = gJsonTerm . unMagic

instance FoldP1 GJsonTerm (M1 _1 _2 _3) => GJsonTerm (M1 _1 _2 _3 _4) where
  gJsonTerm = gJsonResponse

instance GJsonTerm t => GJsonTerm (M.Map T.Text t) where
  gJsonTerm = toJSON . fmap gJsonTerm

instance GJsonTerm t => GJsonTerm [t] where
  gJsonTerm = toJSON . fmap gJsonTerm

instance GJsonTerm Int where
  gJsonTerm = toJSON

instance GJsonTerm Integer where
  gJsonTerm = toJSON

instance GJsonTerm Float where
  gJsonTerm = toJSON

instance GJsonTerm ID where
  gJsonTerm = toJSON

instance GJsonTerm Double where
  gJsonTerm = toJSON

instance GJsonTerm Bool where
  gJsonTerm = toJSON

instance {-# OVERLAPPING #-} GJsonTerm String where
  gJsonTerm = toJSON


