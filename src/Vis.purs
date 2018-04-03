module Vis
  ( module Vis.Types

  , color
  , color1
  , flop
  , removeCoord
  , reorient
  , rotate

  , space
  , leftSpace
  , rightSpace
  , topSpace
  , bottomSpace

  , selectVis
  , selectVisM
  , visDims
  , visInitDec

  , isVisible
  , getColor
  , getHeight
  , getWidth
  ) where

import Color (Color)
import Data.List (List(..), concatMap, (:))
import Data.List.NonEmpty (NonEmptyList, cons, head, singleton, toList, zipWith)
import Data.Map (empty)
import Data.Maybe (Maybe(..), maybe)
import Prelude (flip, map, (<<<), (<>))
import Util (doUnsafeListOp, intersperse)
import V (Decision, Dim, Dir(..), lookupDim)
import Vis.Types (Frame(..), VPs(..), VVis(..), hspace, maybe1, vspace)

--------------------------------------------------------------------------------
-- Transformations

-- | Change the orientation between vertical and horizontal, or angle and radius
reorient :: forall a. VVis a -> VVis a
reorient (Fill f) = Fill (f { vps = swapWH f.vps, frameW = f.frameH, frameH = f.frameW })
reorient (V d l r) = V d (reorient l) (reorient r)
reorient (NextTo v) = NextTo (v { vs = map reorient v.vs })
reorient (Above v) = Above (v { vs = map reorient v.vs })
reorient (MkCartesian v) = MkCartesian (reorient v)
reorient (MkPolar v) = MkPolar (reorient v)
reorient (Overlay v) = Overlay (v { vs = map reorient v.vs })

swapWH :: VPs -> VPs
swapWH (VPs vp) = VPs (vp { width = vp.height, height = vp.width })

-- | Change the direction of composition
flop :: forall a. VVis a -> VVis a
flop (Fill f) = Fill f
flop (V d l r) = V d (flop l) (flop r)
flop (NextTo v) = Above (v { vs = map flop v.vs })
flop (Above v) = NextTo (v { vs = map flop v.vs })
flop (MkCartesian v) = MkCartesian (flop v)
flop (MkPolar v) = MkPolar (flop v)
flop (Overlay v) = Overlay (v { vs = map flop v.vs })

-- | Flop, then reorient
rotate :: forall a. VVis a -> VVis a
rotate = reorient <<< flop

-- | Iterate over a visualization and remove all the constructors that change
-- | the coordinate system.
removeCoord :: forall a. VVis a -> VVis a
removeCoord (Fill f) = Fill f
removeCoord (V d l r) = V d (removeCoord l) (removeCoord r)
removeCoord (NextTo v) = NextTo (v { vs = map removeCoord v.vs })
removeCoord (Above v) = Above (v { vs = map removeCoord v.vs })
removeCoord (MkCartesian v) = removeCoord v
removeCoord (MkPolar v) = removeCoord v
removeCoord (Overlay v) = Overlay (v { vs = map removeCoord v.vs })

space :: VVis Number -> Number -> VVis Number
space (NextTo v) n =
  let newvs = doUnsafeListOp (intersperse (hspace n)) v.vs
  in NextTo (v { vs = newvs })
space (Above v) n =
  let newvs = doUnsafeListOp (intersperse (vspace n)) v.vs
  in Above (v { vs = newvs })
space (V d l r) n = V d (space l n) (space r n)
space (MkCartesian v) n = MkCartesian (space v n)
space (MkPolar v) n = MkPolar (space v n)
space (Overlay v) n =
  let newvs = doUnsafeListOp (intersperse (vspace n)) v.vs
  in Overlay (v { vs = newvs })
space (Fill v) _ = Fill v

leftSpace :: VVis Number -> Number -> VVis Number
leftSpace v n = NextTo { vs: (cons (hspace n) (singleton v))
                       }

rightSpace :: VVis Number -> Number -> VVis Number
rightSpace v n = NextTo { vs: (cons v (singleton (hspace n)))
                        }

topSpace :: VVis Number -> Number -> VVis Number
topSpace v n = Above { vs: (cons (vspace n) (singleton v))
                     }

bottomSpace :: VVis Number -> Number -> VVis Number
bottomSpace v n = Above { vs: (cons v (singleton (vspace n)))
                        }


--------------------------------------------------------------------------------
-- Aesthetics and style functions

color :: forall a. VVis a -> NonEmptyList Color -> VVis a
color (Fill f) cs = let (VPs vps) = f.vps
                    in Fill (f { vps = (VPs (vps { color = head cs } )) })
color (V d l r) cs = V d (color l cs) (color r cs)
color (NextTo v) cs = NextTo (v { vs = zipWith color1 v.vs cs })
color (Above v) cs = Above (v { vs = zipWith color1 v.vs cs })
color (MkCartesian v) cs = MkCartesian (color v cs)
color (MkPolar v) cs = MkPolar (color v cs)
color (Overlay v) cs = Overlay (v { vs = zipWith color1 v.vs cs })

color1 :: forall a. VVis a -> Color -> VVis a
color1 (Fill f) c = let (VPs vps) = f.vps
                    in Fill (f { vps = (VPs (vps { color = c } )) })
color1 (V d l r) c = V d (color1 l c) (color1 r c)
color1 (NextTo v) c = NextTo (v { vs = map (flip color1 c) v.vs })
color1 (Above v) c = Above (v { vs = map (flip color1 c) v.vs })
color1 (MkCartesian v) c = MkCartesian (color1 v c)
color1 (MkPolar v) c = MkPolar (color1 v c)
color1 (Overlay v) c = Overlay (v { vs = map (flip color1 c) v.vs })

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
selectVis dec (Overlay v) = Overlay (v { vs = map (selectVis dec) v.vs })

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
visDims (Overlay v) = concatMap visDims (toList v.vs)

--------------------------------------------------------------------------------
-- Queries

isVisible :: forall e. { vps :: VPs | e } -> Boolean
isVisible r = let (VPs vps) = r.vps
              in vps.visible

getHeight :: VPs -> Number
getHeight (VPs vps) = maybe1 vps.height

getWidth :: VPs -> Number
getWidth (VPs vps) = maybe1 vps.width

getColor :: forall e. { vps :: VPs | e } -> Color
getColor r = let (VPs vps) = r.vps
             in vps.color