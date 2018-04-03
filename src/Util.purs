module Util
  ( convertRange

  , maximum
  , minimum
  , vmaximum
  , vminimum

  , intersperse
  , prependToAll

  , doUnsafeListOp
  , unsafeNonEmpty

  , guessOrientation
  ) where

import Data.List.NonEmpty (concatMap, fromList, toUnfoldable)
import Data.List.Types (List(..), NonEmptyList(..), (:))
import Data.Maybe (fromJust)
import Data.NonEmpty (foldl1)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Ord, max, min, otherwise, ($), (*), (+), (-), (/), (==))
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

-- | Intersperse an element between each pair in a list, borrowed from the
-- | Haskell prelude.
intersperse :: forall a. a -> List a -> List a
intersperse _ Nil = Nil
intersperse sep (x : xs)  = x : prependToAll sep xs

prependToAll :: forall a. a -> List a -> List a
prependToAll _ Nil = Nil
prependToAll sep (x : xs) = sep : x : prependToAll sep xs

doUnsafeListOp :: forall a.
  (List a -> List a) ->
  (NonEmptyList a -> NonEmptyList a)
doUnsafeListOp f ne =
  let l = f (toUnfoldable ne)
  in unsafePartial (fromJust $ fromList l)

unsafeNonEmpty :: forall a. List a -> NonEmptyList a
unsafeNonEmpty l = unsafePartial $ fromJust $ fromList l

guessOrientation :: Number -> Number -> Number
guessOrientation w h | w == 1.0  = h
                     | otherwise = w