module Args where

import GHC.TypeLits

data Arg (name :: Symbol) a = Arg { getArg :: a }
  deriving (Eq, Ord, Show)
