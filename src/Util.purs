module Util
  ( convertRange

  , maximum
  , minimum
  , vmaximum
  , vminimum
  ) where

import Data.List.NonEmpty (concatMap)
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty (foldl1)
import Data.Tuple (Tuple(..))
import Prelude (class Ord, max, min, ($), (*), (+), (-), (/))
import V (plainVals)
import V.Types (V)

-- | Convert a value from one range to another, useful for scaling to a drawing
-- | area.
convertRange :: Number -> Tuple Number Number -> Tuple Number Number -> Number
convertRange v (Tuple omin omax) (Tuple nmin nmax) =
  ((v - omin) / (omax - omin)) * (nmax - nmin) + nmin

-- | Get the maximum value from a nonempty list
maximum :: forall a. (Ord a) => NonEmptyList a -> a
maximum (NonEmptyList xs) = foldl1 max xs

-- | Get the minimum value from a nonempty list
minimum :: forall a. (Ord a) => NonEmptyList a -> a
minimum (NonEmptyList xs) = foldl1 min xs

-- | Get the maximum value from a nonempty list of variational values
vmaximum :: forall a. (Ord a) => NonEmptyList (V a) -> a
vmaximum vs = maximum $ concatMap plainVals vs

-- | Get the minimum value from a nonemtpy list of variational values
vminimum :: forall a. (Ord a) => NonEmptyList (V a) -> a
vminimum vs = minimum $ concatMap plainVals vs