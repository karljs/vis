module Vis
  ( module Vis.Types
  , color
  , color1
  , flop
  , reorient
  , rotate
  , selectVis
  , selectVisM
  , visDims
  , visInitDec
  ) where

import Color (Color)
import Data.List (List(..), concatMap, (:))
import Data.List.NonEmpty (NonEmptyList, head, toList, zipWith)
import Data.Map (empty)
import Data.Maybe (Maybe(..), maybe)
import Prelude (flip, map, (<<<), (<>))
import V (Decision, Dim, Dir(..), leftDec, lookupDim)
import Vis.Types (Orientation(..), VVis(..))

--------------------------------------------------------------------------------
-- Transformations

-- | Change the orientation between vertical and horizontal, or angle and radius
reorient :: forall a. VVis a -> VVis a
reorient (Fill f) = Fill (f { orientation = swapOrientation f.orientation })
reorient (V d l r) = V d (reorient l) (reorient r)
reorient (NextTo v) = NextTo { orientation: swapOrientation v.orientation
                             , vs: map reorient v.vs
                             }
reorient (Above v) = Above { orientation: swapOrientation v.orientation
                           , vs: map reorient v.vs
                           }
reorient (MkCartesian v) = MkCartesian (reorient v)
reorient (MkPolar v) = MkPolar (reorient v)

-- | Change the direction of composition
flop :: forall a. VVis a -> VVis a
flop (Fill f) = Fill f
flop (V d l r) = V d (flop l) (flop r)
flop (NextTo v) = Above (v { vs = map flop v.vs })
flop (Above v) = NextTo (v { vs = map flop v.vs })
flop (MkCartesian v) = MkCartesian (flop v)
flop (MkPolar v) = MkPolar (flop v)

-- | Flop and reorient
rotate :: forall a. VVis a -> VVis a
rotate = reorient <<< flop

swapOrientation :: Orientation -> Orientation
swapOrientation OrientVertical = OrientHorizontal
swapOrientation OrientHorizontal = OrientVertical

--------------------------------------------------------------------------------
-- Aesthetics and style functions

color :: forall a. VVis a -> NonEmptyList Color -> VVis a
color (Fill f) cs = Fill (f { color = head cs })
color (V d l r) cs = V d (color l cs) (color r cs)
color (NextTo v) cs = NextTo (v { vs = zipWith color1 v.vs cs })
color (Above v) cs = Above (v { vs = zipWith color1 v.vs cs })
color (MkCartesian v) cs = MkCartesian (color v cs)
color (MkPolar v) cs = MkPolar (color v cs)

color1 :: forall a. VVis a -> Color -> VVis a
color1 (Fill f) c = Fill (f { color = c })
color1 (V d l r) c = V d (color1 l c) (color1 r c)
color1 (NextTo v) c = NextTo (v { vs = map (flip color1 c) v.vs })
color1 (Above v) c = Above (v { vs = map (flip color1 c) v.vs })
color1 (MkCartesian v) c = MkCartesian (color1 v c)
color1 (MkPolar v) c = MkPolar (color1 v c)

--------------------------------------------------------------------------------
-- Things related to variability

-- | Select with a maybe `Decision`.
selectVisM :: Maybe Decision -> VVis Number -> VVis Number
selectVisM md v = maybe v (flip selectVis v) md


-- | Perform selection on a variational visualization.
selectVis :: forall a. Decision -> VVis a -> VVis a
selectVis _ (Fill f) = Fill f
selectVis dec (V d l r) = case lookupDim d dec of
  Just L -> selectVis dec l
  Just R -> selectVis dec r
  Nothing -> V d (selectVis dec l) (selectVis dec r)
selectVis dec (NextTo v) = NextTo (v { vs = map (selectVis dec) v.vs })
selectVis dec (Above v) = Above (v { vs = map (selectVis dec) v.vs })
selectVis dec (MkCartesian v) = MkCartesian (selectVis dec v)
selectVis dec (MkPolar v) = MkPolar (selectVis dec v)

-- | Generate an initial (view) decision for a particular visualization.
-- | Generally this will be all left selections.
visInitDec :: forall a. VVis a -> Decision
visInitDec v = empty
  -- leftDec (visDims v)

-- | Extract all the dimensions from a visualization
visDims :: forall a. VVis a -> List Dim
visDims (Fill _) = Nil
visDims (V d l r) = d : visDims l <> visDims r
visDims (NextTo v) = concatMap visDims (toList v.vs)
visDims (Above v) = concatMap visDims (toList v.vs)
visDims (MkCartesian v) = visDims v
visDims (MkPolar v) = visDims v