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
type HasJSONResponse record = (Generic (record 'Response), GResToJSON (Rep (record 'Response)))

jsonResponse :: (HasJSONResponse record) => (record 'Response) -> Value
jsonResponse = gResToJSON . from

class GResToJSON (ri :: * -> *) where
    gResToJSON :: ri x -> Value

instance GResToJSON ri
      => GResToJSON (M1 a b ri) where
  gResToJSON (M1 r) = gResToJSON r

instance (GResToJSON ri, GResToJSON rj)
      => GResToJSON (ri :*: rj) where
  gResToJSON (ri :*: rj) = (gResToJSON ri) `combine` (gResToJSON rj)

combine :: Value -> Value -> Value
combine (Object a) (Object b) = Object $ a <> b
combine _ (Object b)          = Object b 
combine (Object a) _          = Object a
combine _ _                   = Object mempty

instance {-# OVERLAPPING #-} (HasJSONResponse record, Typeable record) 
      => GResToJSON (K1 x (M.Map T.Text [record 'Response])) where
    gResToJSON (K1 r) = toJSON $ fmap (jsonResponse <$>) r

instance {-# OVERLAPPING #-}(HasJSONResponse record, Typeable record) 
      => GResToJSON (K1 x (M.Map T.Text (record 'Response))) where
    gResToJSON (K1 r) = toJSON $ jsonResponse <$> r

instance {-# OVERLAPPING #-}(Typeable record, ToJSON record) 
      => GResToJSON (K1 x (M.Map T.Text record)) where
    gResToJSON (K1 r) = toJSON r

instance (HasJSONResponse record, Typeable record)
      => GResToJSON (K1 x (record 'Response)) where
    gResToJSON (K1 r) = jsonResponse r


instance (Typeable record, ToJSON record) => GResToJSON (K1 x record) where
    gResToJSON (K1 r) = object [(T.pack $ show $ typeRep $ Proxy @record) .= r]


