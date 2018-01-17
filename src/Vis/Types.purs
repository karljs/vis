module Vis.Types
  ( Frame(..)
  , VVis(..)
  , above
  , above'
  , fills
  , nextTo
  , nextTo'
  ) where

import Data.Array (toUnfoldable)
import Data.List.NonEmpty (NonEmptyList, foldr, fromList)
import Data.List (List)
import Data.Maybe (fromJust)
import Data.Show (class Show, show)
import Partial.Unsafe (unsafePartial)
import Prelude (map, ($), (<<<), (<>))
import Util (vmaximum, vminimum)
import V (Dim, V(..))

-- | The primary type of a visualization
data VVis a
  = Fill a (Frame a)
  | V Dim (VVis a) (VVis a)
  | NextTo (NonEmptyList (VVis a))
  | Above (NonEmptyList (VVis a))

instance showVVis :: Show a => Show (VVis a) where
  show (Fill v f) = "Fill " <> show v
  show (V d l r) = "V " <> d <> " " <> show l <> " " <> show r
  show (NextTo vs) =
    "NextTo\n" <> (foldr (\v vs -> (v <> "\n" <> vs)) "\n" (map show vs))
  show (Above vs) =
    "Above\n" <> (foldr (\v vs -> (v <> "\n" <> vs)) "\n" (map show vs))

-- | A frame represents the context into which we map the values being charted.
-- | Currently this just tracks the minimum and maximum values in a chart.
data Frame a = Frame
  { frameMax :: a
  , frameMin :: a
  }

instance showFrame :: Show a => Show (Frame a) where
  show (Frame f) = "Frame"

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
nextTo' :: forall a. List (VVis a) -> VVis a
nextTo' vs = NextTo <<< unsafePartial $ fromJust $ fromList vs

nextTo :: forall a. Array (VVis a) -> VVis a
nextTo vs = nextTo' $ toUnfoldable vs

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `Above`.
above' :: forall a. List (VVis a) -> VVis a
above' vs = Above <<< unsafePartial $ fromJust $ fromList vs

-- | An _unsafe_ helper function (when the parameter array is empty) for
-- | composing with `Above`.
above :: forall a. Array (VVis a) -> VVis a
above vs = above' $ toUnfoldable vs