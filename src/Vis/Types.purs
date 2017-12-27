module Vis.Types
  ( Frame(..)
  , Rectangle
  , VVis(..)
  , above
  , fills
  , nextTo
  ) where

import Data.List.NonEmpty (fromList)
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (map, ($), (<<<))
import Util (vmaximum, vminimum)
import V (Dim, V(..))

-- | The primary type of a visualization
data VVis a
  = Fill a (Frame a)
  | V Dim (VVis a) (VVis a)
  | NextTo (NonEmptyList (VVis a))
  | Above (NonEmptyList (VVis a))

-- | A frame represents the context into which we map the values being charted.
-- | Currently this just tracks the minimum and maximum values in a chart.
data Frame a = Frame
  { frameMax :: a
  , frameMin :: a
  }

-- | A rectangle is represented as a top left corner plus a width and height.
type Rectangle = { x :: Number
                 , y :: Number
                 , w :: Number
                 , h :: Number
                 }

-- | An _unsafe_ helper function (when the parameter list is empty) that takes a
-- | list of variational numbers and produces a non-empty list of `Fill`
-- | visualizations.
fills :: List (V Number) -> NonEmptyList (VVis Number)
fills vs =
  let vs' = unsafePartial $ fromJust (fromList vs)
      f = Frame { frameMax: vmaximum vs', frameMin: vminimum vs' }
  in map (vFill f) vs'

-- | For a variational number, product a `Fill` visualization.
vFill :: Frame Number -> V Number -> VVis Number
vFill f (One v) = Fill v f
vFill f (Chc d l r) = V d (vFill f l) (vFill f r)

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `NextTo`.
nextTo :: forall a. List (VVis a) -> VVis a
nextTo vs = NextTo <<< unsafePartial $ fromJust $ fromList vs

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `Above`.
above :: forall a. List (VVis a) -> VVis a
above vs = Above <<< unsafePartial $ fromJust $ fromList vs