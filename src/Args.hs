module Args where

import GHC.TypeLits

data Arg (name :: Symbol) a = Arg { getArg :: a }
  deriving (Eq, Ord, Show)


data Args (ts :: [(Symbol, *)]) where
  ANil :: Args '[]
  (:@@) :: Arg s t -> Args ts -> Args ('(s, t) ': ts)
infixr 5 :@@

instance Show (Args ts) where
  show _ = "Args"

