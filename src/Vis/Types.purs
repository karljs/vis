module Vis.Types
  ( Frame(..)
  , Label(..)
  , LabelPositionH(..)
  , LabelPositionV(..)
  , VPs(..)
  , VVis(..)
  , above
  , above'
  , fills
  , fillsH
  , fillsW
  , nextTo
  , overlay
  , overlayFlat
  , stacks

  , hspace
  , vspace

  , maybe1
  ) where

import Color (Color, white)
import Color.Scheme.MaterialDesign (green)
import Data.Array (toUnfoldable)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList, cons, foldr, fromList, singleton, zipWith)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show (class Show, show)
import Data.String (take)
import Data.Tuple (Tuple(..))
import Math (max, min)
import Partial.Unsafe (unsafePartial)
import Prelude (map, ($), (<>))
import Util (guessOrientation, maximum, minimum, unsafeNonEmpty, vmaximum, vminimum)
import V (Dim, V(..))

-- | The primary type of a visualization
data VVis a
  = Fill { vps :: VPs
         , frameH :: Frame a
         , frameW :: Frame a
         , label :: Maybe Label
         }
  | V Dim (VVis a) (VVis a)
  | NextTo { vs :: (NonEmptyList (VVis a)) }
  | Above { vs :: (NonEmptyList (VVis a)) }
  | MkCartesian (VVis a)
  | MkPolar (VVis a)
  | Overlay { vs :: (NonEmptyList (VVis a)) }
  | Stacked { vs :: (NonEmptyList (VVis a)) }

instance showVVis :: Show a => Show (VVis a) where
  show (Fill f) = "Fill " <> show f.vps <> " "
                          <> "H: " <> show f.frameH <> " "
                          <> "W: " <> show f.frameW <> " "
                          <> "label " <> show f.label
  show (V d l r) = "V " <> d <> " " <> show l <> " " <> show r
  show (NextTo v) =
    "NextTo\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show v.vs))
  show (Above v) =
    "Above\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show v.vs))
  show (MkCartesian v) = "Cartesian\n" <> show v
  show (MkPolar v) = "Polar\n" <> show v
  show (Overlay o) =
    "Overlay\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show o.vs))
  show (Stacked s) =
    "Stacked\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show s.vs))

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
  show (Frame f) = show f.frameMin <> "--" <> show f.frameMax

--------------------------------------------------------------------------------
-- Everything to do with labels

data Label = Label
  { text :: String
  , position :: Tuple LabelPositionV LabelPositionH
  , size :: Number
  }

instance showLabel :: Show Label where
  show (Label l) = "Label: " <> l.text

data LabelPositionV = VPosTop | VPosMiddle | VPosBottom

data LabelPositionH = HPosLeft | HPosMiddle | HPosRight


--------------------------------------------------------------------------------
-- Helper functions for (mostly unsafely) constructing nonempty lists of things

-- | Create fill objects for an array of variational numbers where the data is
-- | bound to the height
fillsH :: Array (V Number) -> NonEmptyList (VVis Number)
fillsH hsarr =
  let hs = toUnfoldable hsarr
      fw = Frame { frameMin: 0.0, frameMax: 1.0 }
  in fills (map (setW 1.0) hs) (genFrame hs) fw

-- | Create fill objects for an array of variational numbers where the data is
-- | bound to the width
fillsW :: Array (V Number) -> NonEmptyList (VVis Number)
fillsW wsarr =
  let ws = toUnfoldable wsarr
      fh = Frame { frameMin: 0.0, frameMax: 1.0 }
  in fills (map (setH 1.0) ws) fh (genFrame ws)

setW :: Number -> V Number -> V (Tuple Number Number)
setW w (Chc d l r) = Chc d (setW w l) (setW w r)
setW w (One h) = One (Tuple w h)

setH :: Number -> V Number -> V (Tuple Number Number)
setH h (Chc d l r) = Chc d (setH h l) (setH h r)
setH h (One w) = One (Tuple w h)

-- | An _unsafe_ helper function (when the parameter list is empty) that takes a
-- | list of variational numbers and produces a frame based on the minimum and
-- | maximum values.
genFrame :: List (V Number) -> Frame Number
genFrame vs =
  let vs'  = unsafePartial $ fromJust (fromList vs)
      fmin = min (vminimum vs') 0.0
      fmax = max (vmaximum vs') 0.0
  in Frame { frameMax: fmax, frameMin: fmin }

-- | For a list of dimensions and a frame, generate a sequence of
fills ::
  List (V (Tuple Number Number)) ->
  Frame Number ->
  Frame Number ->
  NonEmptyList (VVis Number)
fills vs fh fw = unsafeNonEmpty $ map (genFill fh fw) vs

-- | For a variational number, produce a `Fill` visualization.
genFill ::
  Frame Number ->
  Frame Number ->
  V (Tuple Number Number) ->
  VVis Number
genFill fh fw (One (Tuple w h)) =
  let vp = VPs { height: Just h
               , width: Just w
               , color: green
               , visible: true
               }
  in Fill { vps: vp
          , frameH: fh
          , frameW: fw
          , label: Just (defaultLabel (guessOrientation w h))
          }
genFill fh fw (Chc d l r) = V d (genFill fh fw l) (genFill fh fw r)

hspace :: Number -> VVis Number
hspace v = spaceFill (Just v) Nothing

vspace :: Number -> VVis Number
vspace v = spaceFill Nothing (Just v)

spaceFill :: Maybe Number -> Maybe Number -> VVis Number
spaceFill w h =
  let vp = VPs { height: h
               , width: w
               , color: white
               , visible: false }
  in Fill { vps: vp
          , frameH: Frame { frameMin: 0.0, frameMax: 1.0 }
          , frameW: Frame { frameMin: 0.0, frameMax: 1.0 }
          , label: Nothing
          }

defaultLabel :: forall a. Show a => a -> Label
defaultLabel v = Label { text: take 4 (show v)
                       , position: Tuple VPosTop HPosMiddle
                       , size: 36.0 }

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `NextTo`.
nextTo' :: forall a. List (VVis a) -> VVis a
nextTo' vs = NextTo { vs: unsafePartial $ fromJust $ fromList vs
                    }

nextTo :: forall a. Array (VVis a) -> VVis a
nextTo vs = nextTo' $ toUnfoldable vs

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `Above`.
above' :: forall a. List (VVis a) -> VVis a
above' vs = Above { vs: unsafePartial $ fromJust $ fromList vs
                  }

-- | An _unsafe_ helper function (when the parameter array is empty) for
-- | composing with `Above`.
above :: forall a. Array (VVis a) -> VVis a
above vs = above' $ toUnfoldable vs


-- | Overlay the first visualization over the second
overlay :: forall a. VVis a -> VVis a -> VVis a
overlay v1 v2 = Overlay { vs: cons v1 (singleton v2) }

overlayFlat :: forall a. VVis Number -> VVis Number -> VVis Number
overlayFlat v1 v2 =
  let maxH = max (maxValH v1) (maxValH v2)
      maxW = max (maxValW v1) (maxValW v2)
      minH = min (minValH v1) (minValH v2)
      minW = min (minValW v1) (minValW v2)
      v1f = setFrames (Frame { frameMin: minH, frameMax: maxH })
                      (Frame { frameMin: minW, frameMax: maxW })
                      v1
      v2f = setFrames (Frame { frameMin: minH, frameMax: maxH })
                      (Frame { frameMin: minW, frameMax: maxW })
                      v2
  in Overlay { vs: cons v1f (singleton v2f) }

-- | Set or overwrite the frames for an existing visualization.  Parameters
-- | have the height frame first, followed by the width frame.
setFrames :: forall a. Frame a -> Frame a -> VVis a -> VVis a
setFrames fh fw (Fill f) = Fill (f { frameH = fh, frameW = fw })
setFrames fh fw (V d l r) = V d (setFrames fh fw l) (setFrames fh fw r)
setFrames fh fw (NextTo v) = NextTo (v { vs = map (setFrames fh fw) v.vs })
setFrames fh fw (Above v) = Above (v { vs = map (setFrames fh fw) v.vs })
setFrames fh fw (MkCartesian v) = MkCartesian (setFrames fh fw v)
setFrames fh fw (MkPolar v) = MkPolar (setFrames fh fw v)
setFrames fh fw (Overlay v) = Overlay (v { vs = map (setFrames fh fw) v.vs })
setFrames fh fw (Stacked v) = Stacked (v { vs = map (setFrames fh fw) v.vs })

stacks :: Array Number -> Array Number -> NonEmptyList (VVis Number)
stacks xs ys = zipWith stack2 (fillsH (map One xs)) (fillsH (map One ys))

stack2 :: forall a. VVis a -> VVis a -> VVis a
stack2 x y = Stacked { vs: cons x (singleton y) }

--------------------------------------------------------------------------------
-- Queries

maxValH :: VVis Number -> Number
maxValH (Fill f) = let (Frame fr) = f.frameH in fr.frameMax
maxValH (V d l r) = max (maxValH l) (maxValH r)
maxValH (NextTo v) = maximum $ map maxValH v.vs
maxValH (Above v) = maximum $ map maxValH v.vs
maxValH (MkCartesian v) = maxValH v
maxValH (MkPolar v) = maxValH v
maxValH (Overlay v) = maximum $ map maxValH v.vs
maxValH (Stacked v) = maximum $ map minValH v.vs

maxValW :: VVis Number -> Number
maxValW (Fill f) = let (Frame fr) = f.frameW in fr.frameMax
maxValW (V d l r) = max (maxValW l) (maxValW r)
maxValW (NextTo v) = maximum $ map maxValW v.vs
maxValW (Above v) = maximum $ map maxValW v.vs
maxValW (MkCartesian v) = maxValW v
maxValW (MkPolar v) = maxValW v
maxValW (Overlay v) = maximum $ map maxValW v.vs
maxValW (Stacked v) = maximum $ map minValH v.vs

minValH :: VVis Number -> Number
minValH (Fill f) = let (Frame fr) = f.frameH in fr.frameMin
minValH (V d l r) = max (minValH l) (minValH r)
minValH (NextTo v) = minimum $ map minValH v.vs
minValH (Above v) = minimum $ map minValH v.vs
minValH (MkCartesian v) = minValH v
minValH (MkPolar v) = minValH v
minValH (Overlay v) = minimum $ map minValH v.vs
minValH (Stacked v) = minimum $ map minValH v.vs

minValW :: VVis Number -> Number
minValW (Fill f) = let (Frame fr) = f.frameW in fr.frameMin
minValW (V d l r) = max (minValW l) (minValW r)
minValW (NextTo v) = minimum $ map minValW v.vs
minValW (Above v) = minimum $ map minValW v.vs
minValW (MkCartesian v) = minValW v
minValW (MkPolar v) = minValW v
minValW (Overlay v) = minimum $ map minValW v.vs
minValW (Stacked v) = minimum $ map minValW v.vs

maybe1 :: Maybe Number -> Number
maybe1 (Just n) = n
maybe1 _ = 1.0