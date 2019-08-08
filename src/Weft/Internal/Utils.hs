module Weft.Internal.Utils where

import Weft.Internal.Types
import Data.Generic.HKD.Build


buildQuery :: forall rec k. Build rec (ToMagic 'Query) k => k
buildQuery = build @rec @(ToMagic 'Query)

buildResponse :: forall rec k. Build rec (ToMagic 'Response) k => k
buildResponse = build @rec @(ToMagic 'Response)

buildResolver :: forall rec k. Build rec (ToMagic 'Resolver) k => k
buildResolver = build @rec @(ToMagic 'Resolver)

