module Vis.Types
  ( Frame(..)
  , Orientation(..)
  , VVis(..)
  , above
  , above'
  , fills
  , fillsH
  , fillsV
  , nextTo
  ) where

import Data.Array (toUnfoldable)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList, foldr, fromList)
import Data.Maybe (fromJust)
import Data.Show (class Show, show)
import Partial.Unsafe (unsafePartial)
import Prelude (flip, map, ($), (<<<), (<>))
import Util (vmaximum, vminimum)
import V (Dim, V(..))

-- | The primary type of a visualization
data VVis a
  = Fill a (Frame a) Orientation
  | V Dim (VVis a) (VVis a)
  | NextTo (NonEmptyList (VVis a))
  | Above (NonEmptyList (VVis a))

instance showVVis :: Show a => Show (VVis a) where
  show (Fill v f o) = "Fill " <> show v <> " " <> show o
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

data Orientation
  = OrientVertical
  | OrientHorizontal

instance showOrientation :: Show Orientation where
  show OrientVertical = "Vertical"
  show OrientHorizontal = "Horizontal"

-- | An _unsafe_ helper function (when the parameter list is empty) that takes a
-- | list of variational numbers and produces a non-empty list of `Fill`
-- | visualizations.
fills' :: List (V Number) -> Orientation -> NonEmptyList (VVis Number)
fills' vs o =
  let vs' = unsafePartial $ fromJust (fromList vs)
      f = Frame { frameMax: vmaximum vs', frameMin: vminimum vs' }
  in map (vFill f o) vs'

fills :: Array (V Number) -> Orientation -> NonEmptyList (VVis Number)
fills vs o = fills' (toUnfoldable vs) o

fillsV :: Array (V Number) -> NonEmptyList (VVis Number)
fillsV = flip fills OrientVertical

fillsH :: Array (V Number) -> NonEmptyList (VVis Number)
fillsH = flip fills OrientHorizontal

-- | For a variational number, product a `Fill` visualization.
vFill :: Frame Number -> Orientation -> V Number -> VVis Number
vFill f o (One v) = Fill v f o
vFill f o (Chc d l r) = V d (vFill f o l) (vFill f o r)

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
