module Util where

import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty (foldl1)
import Prelude (class Ord, max)

maximum :: forall a. (Ord a) => NonEmptyList a -> a
maximum (NonEmptyList xs) = foldl1 max xs
