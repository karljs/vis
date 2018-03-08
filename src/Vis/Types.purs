module Vis.Types
  ( Frame(..)
  , Label(..)
  , LabelPositionH(..)
  , LabelPositionV(..)
  , Orientation(..)
  , VPs(..)
  , VVis(..)
  , above
  , above'
  , fills
  , fillsH
  , fillsV
  , nextTo
  , overlay
  , overlayFlat

  , hspace
  , vspace

  , maybe1
  ) where

import Color (Color, white)
import Color.Scheme.MaterialDesign (green)
import Data.Array (toUnfoldable)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList, cons, foldr, fromList, singleton)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Math (max, min)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, flip, map, ($), (<>))
import Util (maximum, minimum, vmaximum, vminimum)
import V (Dim, V(..))

-- | The primary type of a visualization
data VVis a
  = Fill { vps :: VPs
         , frame :: Frame a
         , label :: Maybe Label
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
  show (Fill f) = "Fill " <> show f.vps <> " "
                          <> show f.frame
                          <> " label " <> show f.label
  show (V d l r) = "V " <> d <> " " <> show l <> " " <> show r
  show (NextTo v) =
    "NextTo\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show v.vs))
  show (Above v) =
    "Above\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show v.vs))
  show (MkCartesian v) = "Cartesian\n" <> show v
  show (MkPolar v) = "Polar\n" <> show v
  show (Overlay o) =
    "Overlay\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show o.vs))

data VPs = VPs
  { height :: Maybe Number
  , width :: Maybe Number
  , color :: Color
  , visible :: Boolean
  }

instance showVPs :: Show VPs where
  show (VPs vp) = "(" <> show vp.height <> "," <> show vp.width <> ")"

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
  let vs'  = unsafePartial $ fromJust (fromList vs)
      fmin = min (vminimum vs') 0.0
      fmax = max (vmaximum vs') 0.0
      frm  = Frame { frameMax: fmax
                   , frameMin: fmin }
  in map (vFill frm o) vs'

fills :: Array (V Number) -> Orientation -> NonEmptyList (VVis Number)
fills vs o = fills' (toUnfoldable vs) o

fillsV :: Array (V Number) -> NonEmptyList (VVis Number)
fillsV = flip fills OrientVertical

fillsH :: Array (V Number) -> NonEmptyList (VVis Number)
fillsH = flip fills OrientHorizontal

-- | For a variational number, produce a `Fill` visualization.
vFill :: Frame Number -> Orientation -> V Number -> VVis Number
vFill f o (One v) =
  let (Tuple h w) = case o of
                      OrientVertical -> Tuple (Just v) (Nothing)
                      OrientHorizontal -> Tuple Nothing (Just v)
      vp = VPs { height: h
               , width: w
               , color: green
               , visible: true
               }
  in Fill { vps: vp
          , frame: f
          , label: Just (defaultLabel v)
          }
vFill f o (Chc d l r) = V d (vFill f o l) (vFill f o r)

hspace :: Number -> VVis Number
hspace n = spaceFill n OrientHorizontal

vspace :: Number -> VVis Number
vspace n = spaceFill n OrientVertical

spaceFill :: Number -> Orientation -> VVis Number
spaceFill n o =
  let (Tuple h w) = case o of
                      OrientVertical -> (Tuple (Just n) Nothing)
                      OrientHorizontal -> (Tuple Nothing (Just n))
      vp = VPs { height: h
               , width: w
               , color: white
               , visible: false }
  in Fill { vps: vp
          , frame: Frame { frameMin: 0.0, frameMax: 1.0 }
          , label: Nothing
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

overlayFlat :: forall a. VVis Number -> VVis Number -> VVis Number
overlayFlat v1 v2 = let mx = max (maxVal v1) (maxVal v2)
                        mn = min (minVal v1) (minVal v2)
                        v1f = setFrame (Frame { frameMin: mn, frameMax: mx }) v1
                        v2f = setFrame (Frame { frameMin: mn, frameMax: mx }) v2
                    in Overlay { vs: cons v1f (singleton v2f) }

setFrame :: forall a. Frame a -> VVis a -> VVis a
setFrame fr (Fill f) = Fill (f { frame = fr })
setFrame fr (V d l r) = V d (setFrame fr l) (setFrame fr r)
setFrame fr (NextTo v) = NextTo (v { vs = map (setFrame fr) v.vs })
setFrame fr (Above v) = Above (v { vs = map (setFrame fr) v.vs })
setFrame fr (MkCartesian v) = MkCartesian (setFrame fr v)
setFrame fr (MkPolar v) = MkPolar (setFrame fr v)
setFrame fr (Overlay v) = Overlay (v { vs = map (setFrame fr) v.vs })

--------------------------------------------------------------------------------
-- Queries

maxVal :: VVis Number -> Number
maxVal (Fill f) = let (Frame fr) = f.frame in fr.frameMax
maxVal (V d l r) = max (maxVal l) (maxVal r)
maxVal (NextTo v) = maximum $ map maxVal v.vs
maxVal (Above v) = maximum $ map maxVal v.vs
maxVal (MkCartesian v) = maxVal v
maxVal (MkPolar v) = maxVal v
maxVal (Overlay v) = maximum $ map maxVal v.vs

minVal :: VVis Number -> Number
minVal (Fill f) = let (Frame fr) = f.frame in fr.frameMin
minVal (V d l r) = min (minVal l) (minVal r)
minVal (NextTo v) = minimum $ map minVal v.vs
minVal (Above v) = minimum $ map minVal v.vs
minVal (MkCartesian v) = minVal v
minVal (MkPolar v) = minVal v
minVal (Overlay v) = minimum $ map minVal v.vs

maybe1 :: Maybe Number -> Number
maybe1 (Just n) = n
maybe1 _ = 1.0