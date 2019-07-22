module Weft.Generics.RecordGen
  ( HasRecordGen
  , recordGen
  ) where

import Test.QuickCheck
import GHC.Generics
import Weft.Internal.Types


type HasRecordGen record (ts :: TypeState) =
  ( Generic (record ts)
  , GRecordGen (Rep (record ts))
  )

recordGen :: (HasRecordGen record ts) => Gen (record ts)
recordGen = to <$> gRecordGen


class GRecordGen r where
  gRecordGen :: Gen (r x)

instance GRecordGen r => GRecordGen (M1 _1 _2 r) where
  gRecordGen = M1 <$> gRecordGen

instance (GRecordGen r1, GRecordGen r2) => GRecordGen (r1 :*: r2) where
  gRecordGen = (:*:) <$> gRecordGen
                     <*> gRecordGen

instance Arbitrary a => GRecordGen (K1 _1 a) where
  gRecordGen = K1 <$> arbitrary

instance {-# OVERLAPPING #-} HasRecordGen r ts => GRecordGen (K1 _1 (r ts)) where
  gRecordGen = K1 <$> recordGen

instance {-# OVERLAPPING #-} HasRecordGen r ts => GRecordGen (K1 _1 (Maybe (r ts))) where
  gRecordGen = fmap K1 . sized $ \n ->
    case n <= 0 of
      True  -> pure Nothing
      False -> Just <$> resize (n - 1) recordGen

