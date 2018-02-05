module Canvas.Drawing
  ( parseVis

  , splitBoxHEven
  , splitBoxHOdd
  , splitBoxVEven
  , splitBoxVOdd
  , splitWedgeHEven
  , splitWedgeHOdd
  , splitWedgeVEven
  , splitWedgeVOdd

  , toCartesian
  , toPolar
  , toRectangle
  , toWedge
  ) where

import Canvas.Drawing.Polar (drawHintWedge, drawWedgeH, drawWedgeV)
import Canvas.Drawing.Rectangular (drawBarH, drawBarV, drawHintRect)
import Canvas.Types (CEffects, Rectangle(..), Space(..), Wedge(..))
import Color (Color, black)
import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_, sum)
import Data.Int (toNumber)
import Data.List (List(..), zipWith, (:))
import Data.List.NonEmpty (length, toList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Graphics.Canvas (Context2D)
import Math (pi)
import Prelude (Unit, discard, map, min, ($), (*), (+), (-), (/))
import UI (DecisionColors)
import V (Decision, Dir(..), lookupDim)
import Vis.Types (Orientation(..), VVis(..))

-- | The main entry point for rendering a visualization by parsing it and
-- | recursively diving up the space.
parseVis :: forall m.
  Context2D ->
  Decision ->
  DecisionColors ->
  VVis Number ->
  Space ->
  Eff (CEffects m) Unit
parseVis ctx dec cs (NextTo v) (Cartesian r) = do
  let bs = case v.orientation of
             OrientVertical ->
               splitBoxHEven r (length v.vs)
             OrientHorizontal ->
               splitBoxHOdd r (toList $ map (relativeSize dec) v.vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList v.vs) bs
parseVis ctx dec cs (NextTo v) (Polar w) = do
  let ws = case v.orientation of
             OrientVertical ->
               splitWedgeHEven w (length v.vs)
             OrientHorizontal ->
               splitWedgeHOdd w (toList $ map (relativeSize dec) v.vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList v.vs) ws

parseVis ctx dec cs (Above v) (Cartesian r) = do
  let bs = case v.orientation of
             OrientVertical ->
               splitBoxVOdd r (toList $ map (relativeSize dec) v.vs)
             OrientHorizontal ->
               splitBoxVEven r (length v.vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList v.vs) bs
parseVis ctx dec cs (Above v) (Polar w) = do
  let ws = case v.orientation of
             OrientVertical ->
               splitWedgeVOdd w (toList $ map (relativeSize dec) v.vs)
             OrientHorizontal ->
               splitWedgeVEven w (length v.vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList v.vs) ws

parseVis ctx dec cs (V d l r) sp = do
  case lookupDim d dec of
    Just L -> parseVis ctx dec cs l sp
    Just R -> parseVis ctx dec cs r sp
    _      -> parseVis ctx dec cs l sp
  case lookup d cs of
    Just col -> drawVHint ctx col sp
    _        -> drawVHint ctx black sp

parseVis ctx dec cs (MkCartesian v) s = parseVis ctx dec cs v (toCartesian s)
parseVis ctx dec cs (MkPolar v)     s = parseVis ctx dec cs v (toPolar s)

parseVis ctx dec cs (Fill f) (Cartesian r) =
  case f.orientation of
    OrientVertical -> drawBarV ctx f.val r f.frame f.label f.color
    OrientHorizontal -> drawBarH ctx f.val r f.frame f.label f.color
parseVis ctx dec cs (Fill f) (Polar w) =
  case f.orientation of
    OrientVertical -> drawWedgeV ctx f.val w f.frame f.label f.color
    OrientHorizontal -> drawWedgeH ctx f.val w f.frame f.label f.color


-- | Draw some visual indicator that part of a chart contains variability.
drawVHint :: forall m. Context2D -> Color -> Space -> Eff (CEffects m) Unit
drawVHint ctx col (Cartesian r) = drawHintRect ctx col r
drawVHint ctx col (Polar w) = drawHintWedge ctx col w

relativeSize :: Decision -> VVis Number -> Number
relativeSize _ (Fill f) = abs f.val
relativeSize dec (V d l r) =
  case lookupDim d dec of
    Just L -> relativeSize dec l
    Just R -> relativeSize dec r
    _      -> 1.0
relativeSize _ _ = 1.0

--------------------------------------------------------------------------------
-- Functions related to splitting spaces

-- | Divide a rectangular space into equal horizontal chunks.
splitBoxHEven :: Rectangle -> Int -> List Space
splitBoxHEven _ 0 = Nil
splitBoxHEven (Rectangle r) i =
  let newW = r.w / toNumber i
  in Cartesian (Rectangle (r { w = newW })) :
       splitBoxHEven (Rectangle (r { x = r.x + newW, w = r.w - newW })) (i - 1)

-- | Divide a rectangular space into equal horizontal chunks.
splitBoxHOdd :: Rectangle -> List Number -> List Space
splitBoxHOdd _ _ = Nil

-- | Divide a rectangular space into equal vertical chunks.
splitBoxVEven :: Rectangle -> Int -> List Space
splitBoxVEven _ 0 = Nil
splitBoxVEven (Rectangle r) i =
  let newH = r.h / toNumber i
  in Cartesian (Rectangle (r { h = newH })) :
       splitBoxVEven (Rectangle (r { y = r.y + newH, h = r.h - newH })) (i - 1)

splitBoxVOdd :: Rectangle -> List Number -> List Space
splitBoxVOdd _ _ = Nil

-- | Divide a wedge into equal sub-spaces by angle such as for a coxcomb
-- | chart.
splitWedgeHEven :: Wedge -> Int -> List Space
splitWedgeHEven _ 0 = Nil
splitWedgeHEven (Wedge w) i =
  let newA = ((w.endAngle - w.startAngle) / toNumber i) + w.startAngle
  in Polar (Wedge (w { endAngle = newA })) :
       splitWedgeHEven (Wedge (w { startAngle = newA })) (i - 1)

-- | Divide a wedge into unequal sub-spaces by angle such as for a pie chart.
splitWedgeHOdd :: Wedge -> List Number -> List Space
splitWedgeHOdd w ls = split w ls (sum ls)
  where split :: Wedge -> List Number -> Number -> List Space
        split _ Nil _ = Nil
        split (Wedge w) (v : vs) t =
          let newAngle = (v / t) * (w.endAngle - w.startAngle) + w.startAngle
          in Polar (Wedge (w { endAngle = newAngle })) :
               split (Wedge (w { startAngle = newAngle })) vs (t - v)

-- | Divide a wedge into equal sub-spaces by radius
splitWedgeVEven :: Wedge -> Int -> List Space
splitWedgeVEven w i = Nil

splitWedgeVOdd :: Wedge -> List Number -> List Space
splitWedgeVOdd w ls = Nil

--------------------------------------------------------------------------------
-- Functions for converting between polar and Cartesian coordinate systems

-- | Convert some chunk of space to a Cartesian coordinate system
toCartesian :: Space -> Space
toCartesian (Polar w) = Cartesian (toRectangle w)
toCartesian s = s

-- | Convert a wedge to a rectangle contained entirely within it
-- TODO THIS IS ACTUALLY INCORRECT
toRectangle :: Wedge -> Rectangle
toRectangle (Wedge w) = Rectangle { x: w.x - w.outRad
                                  , y: w.y - w.outRad
                                  , w: w.outRad * 2.0
                                  , h: w.outRad * 2.0
                                  }
toPolar :: Space -> Space
toPolar (Cartesian r) = Polar (toWedge r)
toPolar s = s

toWedge :: Rectangle -> Wedge
toWedge (Rectangle r) = Wedge { x: (r.x + r.w) / 2.0
                              , y: (r.y + r.h) / 2.0
                              , inRad: 0.0
                              , outRad: (min r.h r.w) / 2.0
                              , startAngle: 0.0
                              , endAngle: 2.0 * pi
                              }
