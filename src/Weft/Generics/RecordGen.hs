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
import           GHC.Generics
import           Test.QuickCheck hiding (Args)
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

magicRecordGen :: HasMagicRecordGen record ts => Gen (J' record ts)
magicRecordGen = gRecordGen


class GRecordGen (r :: * -> *) where
  gRecordGen :: Gen (r x)

instance GRecordGen r => GRecordGen (M1 _1 _2 r) where
  gRecordGen = M1 <$> gRecordGen

instance (GRecordGen r1, GRecordGen r2) => GRecordGen (r1 :*: r2) where
  gRecordGen = (:*:) <$> gRecordGen
                     <*> gRecordGen

instance TermGen a => GRecordGen (K1 _1 a) where
  gRecordGen = K1 <$> termGen

class TermGen (t :: *) where
  termGen :: Gen t

instance TermGen Text where
  termGen = arbitrary

instance TermGen Int where
  termGen = arbitrary

instance TermGen Integer where
  termGen = arbitrary

instance {-# OVERLAPPING #-} TermGen String where
  termGen = arbitrary

instance TermGen Float where
  termGen = arbitrary

instance TermGen Double where
  termGen = arbitrary

instance TermGen ID where
  termGen = arbitrary

instance TermGen () where
  termGen = arbitrary

instance TermGen (Magic ts t) => TermGen (ToMagic ts t) where
  termGen = ToMagic <$> termGen

instance GRecordGen (M1 _1 _2 _3) => TermGen (M1 _1 _2 _3 _4) where
  termGen = gRecordGen

instance TermGen a => TermGen [a] where
  termGen = sized $ \n ->
    case n <= 0 of
      True  -> pure []
      False -> resize (n - 1) $ listOf termGen

instance HasRecordGen r ts => TermGen (r ts) where
  termGen = scale (max 0 . subtract 1) recordGen

instance TermGen a => TermGen (Maybe a) where
  termGen = sized $ \n ->
    case n <= 0 of
      True  -> pure Nothing
      False -> Just <$> resize (n - 1) termGen

instance TermGen t => TermGen (M.Map Text t) where
  termGen = sized $ \n ->
    case n <= 0 of
      True  -> pure M.empty
      False -> M.singleton <$> termGen
                           <*> resize (n `div` 5) termGen

instance (Arbitrary (Args args), TermGen t) => TermGen (Args args, t) where
  termGen = (,) <$> arbitrary <*> termGen


-- TODO(sandy): bad orphan bad!
instance Arbitrary Text where
  arbitrary = fmap T.pack $ arbitrary `suchThat` \s ->
    and [ all (\c -> isAlphaNum c && fromEnum c <= 100) s
        , not $ null s
        , not . isDigit $ head s
        ]

