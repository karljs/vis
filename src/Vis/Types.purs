module Vis.Types
  ( Frame(..)
  , Label(..)
  , LabelPositionH(..)
  , LabelPositionV(..)
  , Orientation(..)
  , VVis(..)
  , above
  , above'
  , fills
  , fillsH
  , fillsV
  , nextTo
  , overlay
  , spaceFillV
  ) where

import Color (Color, white)
import Color.Scheme.MaterialDesign (green)
import Data.Array (toUnfoldable)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList, cons, foldr, fromList, singleton)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, flip, map, ($), (<>))
import Util (vmaximum, vminimum)
import V (Dim, V(..))

-- | The primary type of a visualization
data VVis a
  = Fill { val :: a
         , frame :: Frame a
         , orientation :: Orientation
         , label :: Maybe Label
         , color :: Color
         }
  | V Dim (VVis a) (VVis a)
  | NextTo { orientation :: Orientation
           , vs :: (NonEmptyList (VVis a))
           }
  | Above { orientation :: Orientation
          , vs :: (NonEmptyList (VVis a))
          }
  | MkCartesian (VVis a)
  | MkPolar (VVis a)
  | Overlay { vs :: (NonEmptyList (VVis a)) }

instance showVVis :: Show a => Show (VVis a) where
  show (Fill f) = "Fill " <> show f.val <> " "
                          <> show f.frame <> " "
                          <> show f.orientation <> " label "
                          <> show f.label
  show (V d l r) = "V " <> d <> " " <> show l <> " " <> show r
  show (NextTo v) =
    "NextTo\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show v.vs))
  show (Above v) =
    "Above\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show v.vs))
  show (MkCartesian v) = "Cartesian\n" <> show v
  show (MkPolar v) = "Polar\n" <> show v
  show (Overlay o) =
    "Overlay\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show o.vs))

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

instance eqOrientation :: Eq Orientation where
  eq OrientVertical OrientVertical = true
  eq OrientHorizontal OrientHorizontal = true
  eq _ _ = false

--------------------------------------------------------------------------------
-- Everything to do with labels

data Label = Label
  { text :: String
  , position :: Tuple LabelPositionV LabelPositionH
  , size :: Number
  }

instance showLabel :: Show Label where
  show (Label l) = "Label: " <> l.text <> " " <> show l.size

data LabelPositionV = VPosTop | VPosMiddle | VPosBottom

data LabelPositionH = HPosLeft | HPosMiddle | HPosRight


--------------------------------------------------------------------------------
-- Helper functions for (mostly unsafely) constructing nonempty lists of things

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
vFill f o (One v) =
  Fill { val: v
       , frame: f
       , orientation: o
       , label: Just (defaultLabel v)
       , color: green
       }
vFill f o (Chc d l r) = V d (vFill f o l) (vFill f o r)

spaceFillV :: Number -> VVis Number
spaceFillV n = Fill { val: 0.0
                   , frame: Frame { frameMin: 0.0, frameMax: 1.0 }
                   , orientation: OrientVertical
                   , label: Nothing
                   , color: white
                   }

defaultLabel :: forall a. Show a => a -> Label
defaultLabel v = Label { text: show v
                       , position: Tuple VPosTop HPosMiddle
                       , size: 36.0 }

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `NextTo`.
nextTo' :: forall a. List (VVis a) -> VVis a
nextTo' vs = NextTo { orientation: OrientVertical
                    , vs: unsafePartial $ fromJust $ fromList vs
                    }

nextTo :: forall a. Array (VVis a) -> VVis a
nextTo vs = nextTo' $ toUnfoldable vs

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `Above`.
above' :: forall a. List (VVis a) -> VVis a
above' vs = Above { orientation: OrientHorizontal
                  , vs: unsafePartial $ fromJust $ fromList vs
                  }

-- | An _unsafe_ helper function (when the parameter array is empty) for
-- | composing with `Above`.
above :: forall a. Array (VVis a) -> VVis a
above vs = above' $ toUnfoldable vs


-- | Overlay the first visualization over the second
overlay :: forall a. VVis a -> VVis a -> VVis a
overlay v1 v2 = Overlay { vs: cons v1 (singleton v2) }