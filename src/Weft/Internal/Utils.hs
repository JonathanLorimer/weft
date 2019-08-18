module Weft.Internal.Utils where

import Data.Char
import Weft.Internal.Types
import Data.Generic.HKD.Build
import GHC.TypeLits
import Data.Proxy


buildQuery :: forall rec k. Build rec (ToMagic 'Query) k => k
buildQuery = build @rec @(ToMagic 'Query)

buildResponse :: forall rec k. Build rec (ToMagic 'Response) k => k
buildResponse = build @rec @(ToMagic 'Response)

buildResolver :: forall rec k. Build rec (ToMagic 'Resolver) k => k
buildResolver = build @rec @(ToMagic 'Resolver)


uncamelSym :: forall sym. KnownSymbol sym => String
uncamelSym = uncamel . symbolVal $ Proxy @sym


uncamel :: String -> String
uncamel s
  | all isLower s = s
  | otherwise = lowerHead $ dropWhile isLower s
  where
    lowerHead [] = []
    lowerHead (a : as) = toLower a : as

