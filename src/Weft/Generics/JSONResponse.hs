module Weft.Generics.JSONResponse
  ( HasJSONResponse
  , HasMagicJSONResponse
  , jsonResponse
  , magicJsonResponse
  , combine
  ) where

import           Weft.Internal.Types
import           GHC.Generics
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as M


------------------------------------------------------------------------------
-- |
type HasJSONResponse record =
  ( Generic (record 'Response)
  , GJsonResponse (Rep (record 'Response))
  )

type HasMagicJSONResponse record =
  ( Generic record
  , GJsonResponse (J record 'Response)
  )


jsonResponse :: (HasJSONResponse record) => (record 'Response) -> Value
jsonResponse = gJsonResponse . from

magicJsonResponse :: (HasMagicJSONResponse record) => (HKD record (ToMagic 'Response)) -> Value
magicJsonResponse = gJsonResponse . runHKD

class GJsonResponse (ri :: * -> *) where
    gJsonResponse :: ri x -> Value

instance GJsonResponse ri
      => GJsonResponse (M1 a b ri) where
  gJsonResponse (M1 r) = gJsonResponse r

instance (GJsonResponse ri, GJsonResponse rj)
      => GJsonResponse (ri :*: rj) where
  gJsonResponse (ri :*: rj) = (gJsonResponse ri) `combine` (gJsonResponse rj)

combine :: Value -> Value -> Value
combine (Object a) (Object b) = Object $ a <> b
combine _ _                   = error "combine failed in JSONResponse, this should not have happened"

class GJsonTerm (t :: *) where
  gJsonTerm :: t -> Value

instance GJsonTerm t => GJsonResponse (K1 x t) where
  gJsonResponse (K1 t) = gJsonTerm t

instance GJsonTerm (Magic ts t) => GJsonTerm (ToMagic ts t) where
  gJsonTerm = gJsonTerm . unMagic

instance GJsonResponse (M1 _1 _2 _3) => GJsonTerm (M1 _1 _2 _3 _4) where
  gJsonTerm = gJsonResponse

instance GJsonTerm t => GJsonTerm (M.Map T.Text t) where
  gJsonTerm = toJSON . fmap gJsonTerm

instance GJsonTerm t => GJsonTerm [t] where
  gJsonTerm = toJSON . fmap gJsonTerm

instance HasJSONResponse record => GJsonTerm (record 'Response) where
  gJsonTerm = jsonResponse

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


