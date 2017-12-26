module Util where

import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty (foldl1)
import Prelude (class Ord, max, min)

maximum :: forall a. (Ord a) => NonEmptyList a -> a
maximum (NonEmptyList xs) = foldl1 max xs

minimum :: forall a. (Ord a) => NonEmptyList a -> a
minimum (NonEmptyList xs) = foldl1 min xs