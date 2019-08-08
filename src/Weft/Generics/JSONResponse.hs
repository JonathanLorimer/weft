module Weft.Generics.JSONResponse (
  HasJSONResponse,
  jsonResponse
) where

import Weft.Internal.Types
import           GHC.Generics
import           Data.Aeson
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Map as M


------------------------------------------------------------------------------
-- |
type HasJSONResponse record = (Generic (record 'Response), GJsonResponse (Rep (record 'Response)))

jsonResponse :: (HasJSONResponse record) => (record 'Response) -> Value
jsonResponse = gJsonResponse . from

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

instance {-# OVERLAPPING #-} (HasJSONResponse record, Typeable record)
      => GJsonResponse (K1 x (M.Map T.Text [record 'Response])) where
    gJsonResponse (K1 r) = toJSON $ fmap jsonResponse <$> r

instance {-# OVERLAPPING #-} (HasJSONResponse record, Typeable record)
      => GJsonResponse (K1 x (M.Map T.Text (record 'Response))) where
    gJsonResponse (K1 r) = toJSON $ jsonResponse <$> r

instance {-# OVERLAPPING #-} (Typeable record, ToJSON record)
      => GJsonResponse (K1 x (M.Map T.Text record)) where
    gJsonResponse (K1 r) = toJSON r


