module Weft.Generics.RecordGen
  ( HasRecordGen
  , HasMagicRecordGen
  , recordGen
  , magicRecordGen
  ) where

import           Data.Char
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           GHC.Generics
import           Test.QuickCheck
import           Weft.Internal.Types


type HasRecordGen record (ts :: TypeState) =
  ( Generic (record ts)
  , GRecordGen (Rep (record ts))
  )

type HasMagicRecordGen (record :: *) (ts :: TypeState) =
  ( Generic record
  , GRecordGen (J record ts)
  )

recordGen :: (HasRecordGen record ts) => Gen (record ts)
recordGen = to <$> gRecordGen

magicRecordGen :: HasMagicRecordGen record ts => Gen (J record ts Void)
magicRecordGen = gRecordGen


class GRecordGen (r :: * -> *) where
  gRecordGen :: Gen (r x)

instance GRecordGen r => GRecordGen (M1 _1 _2 r) where
  gRecordGen = M1 <$> gRecordGen

instance (GRecordGen r1, GRecordGen r2) => GRecordGen (r1 :*: r2) where
  gRecordGen = (:*:) <$> gRecordGen
                     <*> gRecordGen

instance {-# OVERLAPPING #-} GRecordGen (K1 _1 (Magic t ts)) => GRecordGen (K1 _1 (ToMagic t ts)) where
  gRecordGen = fmap (K1 . ToMagic . unK1) $ gRecordGen @(K1 _1 (Magic t ts))

instance Arbitrary a => GRecordGen (K1 _1 a) where
  gRecordGen = K1 <$> scale (`div` 5) arbitrary

instance {-# OVERLAPPING #-} GRecordGen (K1 _1 Text) where
  gRecordGen = K1 <$> arbitrary @Text

instance {-# OVERLAPPING #-} HasRecordGen r ts => GRecordGen (K1 _1 (r ts)) where
  gRecordGen = K1 <$> scale (subtract 1) recordGen

instance {-# OVERLAPPING #-} HasRecordGen r ts => GRecordGen (K1 _1 (Maybe (r ts))) where
  gRecordGen = fmap K1 . sized $ \n ->
    case n <= 0 of
      True  -> pure Nothing
      False -> Just <$> resize (n - 1) recordGen

instance {-# OVERLAPPING #-} HasRecordGen r ts => GRecordGen (K1 _1 (M.Map Text (r ts))) where
  gRecordGen = fmap K1 . sized $ \n ->
    case n <= 0 of
      True  -> pure M.empty
      False -> M.singleton <$> arbitrary @Text
                           <*> resize (n `div` 5) recordGen


-- TODO(sandy): bad orphan bad!
instance Arbitrary Text where
  arbitrary = fmap T.pack $ arbitrary `suchThat` \s ->
    and [ all (\c -> isAlphaNum c && fromEnum c <= 100) s
        , not $ null s
        , not . isDigit $ head s
        ]

