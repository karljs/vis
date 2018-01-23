module Vis
  ( module Vis.Types
  , reorient
  , rotate
  , selectVis
  , selectVisM
  , visDims
  , visInitDec
  ) where

import Data.List (List(..), concatMap, (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..), maybe)
import Prelude (flip, map, (<>))
import V (Decision, Dim, Dir(..), leftDec, lookupDim)
import Vis.Types (Orientation(..), VVis(..))

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

rotate :: forall a. VVis a -> VVis a
rotate (Fill f) = Fill f
rotate (V d l r) = V d (rotate l) (rotate r)
rotate (NextTo v) = Above (v { vs = map rotate v.vs })
rotate (Above v) = NextTo (v { vs = map rotate v.vs })
rotate (MkCartesian v) = MkCartesian (rotate v)
rotate (MkPolar v) = MkPolar (rotate v)

swapOrientation :: Orientation -> Orientation
swapOrientation OrientVertical = OrientHorizontal
swapOrientation OrientHorizontal = OrientVertical

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
visInitDec v = leftDec (visDims v)

-- | Extract all the dimensions from a visualization
visDims :: forall a. VVis a -> List Dim
visDims (Fill _) = Nil
visDims (V d l r) = d : visDims l <> visDims r
visDims (NextTo v) = concatMap visDims (toList v.vs)
visDims (Above v) = concatMap visDims (toList v.vs)
visDims (MkCartesian v) = visDims v
visDims (MkPolar v) = visDims v