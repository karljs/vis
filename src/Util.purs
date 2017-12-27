module Util where

import Data.List.NonEmpty (concatMap)
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty (foldl1)
import Prelude (class Ord, max, min, ($))
import V (plainVals)
import V.Types (V)

maximum :: forall a. (Ord a) => NonEmptyList a -> a
maximum (NonEmptyList xs) = foldl1 max xs

vmaximum :: forall a. (Ord a) => NonEmptyList (V a) -> a
vmaximum vs = maximum $ concatMap plainVals vs

minimum :: forall a. (Ord a) => NonEmptyList a -> a
minimum (NonEmptyList xs) = foldl1 min xs

vminimum :: forall a. (Ord a) => NonEmptyList (V a) -> a
vminimum vs = minimum $ concatMap plainVals vs