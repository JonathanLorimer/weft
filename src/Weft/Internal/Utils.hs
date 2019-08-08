module Weft.Internal.Utils where

import Weft.Internal.Types
import Data.Generic.HKD.Build


buildQuery :: forall rec k. Build rec (ToMagic 'Query) k => k
buildQuery = build @rec @(ToMagic 'Query)

