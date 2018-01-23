module Canvas.Drawing
  ( parseVis

  , splitBoxH
  , splitBoxV
  , splitWedgeHEven
  , splitWedgeHOdd
  , splitWedgeV

  , toCartesian
  , toPolar
  , toRectangle
  , toWedge
  ) where

import Canvas.Drawing.Polar (drawHintWedge, drawWedgeV)
import Canvas.Drawing.Rectangular (drawBarH, drawBarV, drawHintRect)
import Canvas.Types (CEffects, Rectangle(..), Space(..), Wedge(..))
import Color (Color, black)
import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.List (List(..), zipWith, (:))
import Data.List.NonEmpty (length, toList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Context2D)
import Math (pi)
import Prelude (Unit, bind, min, pure, unit, ($), (*), (+), (-), (/))
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
parseVis ctx dec cs (NextTo vs) (Cartesian r) = do
  let bs = splitBoxH r (length vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) bs
parseVis ctx dec cs (NextTo vs) (Polar w) = do
  let ws = splitWedgeHEven w (length vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) ws

parseVis ctx dec cs (Above vs) (Cartesian r) = do
  let bs = splitBoxV r (length vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) bs
parseVis ctx dec cs (Above vs) (Polar w) = pure unit

parseVis ctx dec cs (V d l r) sp = do
  _ <- case lookup d cs of
         Just col -> drawVHint ctx col sp
         _ -> drawVHint ctx black sp
  case lookupDim d dec of
    Just L -> parseVis ctx dec cs l sp
    Just R -> parseVis ctx dec cs r sp
    _      -> parseVis ctx dec cs l sp

parseVis ctx dec cs (MkCartesian v) s = parseVis ctx dec cs v (toCartesian s)
parseVis ctx dec cs (MkPolar v)     s = parseVis ctx dec cs v (toPolar s)

parseVis ctx dec cs (Fill f) (Cartesian r) =
  case f.fillOrientation of
    OrientVertical -> drawBarV ctx f.fillVal r f.fillFrame f.fillLabel
    OrientHorizontal -> drawBarH ctx f.fillVal r f.fillFrame f.fillLabel
parseVis ctx dec cs (Fill f) (Polar w) =
  case f.fillOrientation of
    OrientVertical -> drawWedgeV ctx f.fillVal w f.fillFrame f.fillLabel
    OrientHorizontal -> pure unit -- drawWedgeV ctx f.fillVal r f.fillFrame f.fillLabel

-- | Draw some visual indicator that part of a chart contains variability.
drawVHint :: forall m. Context2D -> Color -> Space -> Eff (CEffects m) Unit
drawVHint ctx col (Cartesian r) = drawHintRect ctx col r
drawVHint ctx col (Polar w) = drawHintWedge ctx col w


--------------------------------------------------------------------------------
-- Functions related to splitting spaces

-- | Divide a rectangular space into equal horizontal chunks.
splitBoxH :: Rectangle -> Int -> List Space
splitBoxH _ 0 = Nil
splitBoxH (Rectangle r) i =
  let newW = r.w / toNumber i
  in Cartesian (Rectangle (r { w = newW })) :
       splitBoxH (Rectangle (r { x = r.x + newW, w = r.w - newW })) (i - 1)

-- | Divide a rectangular space into equal vertical chunks.
splitBoxV :: Rectangle -> Int -> List Space
splitBoxV _ 0 = Nil
splitBoxV (Rectangle r) i =
  let newH = r.h / toNumber i
  in Cartesian (Rectangle (r { h = newH })) :
       splitBoxV (Rectangle (r { y = r.y + newH, h = r.h - newH })) (i - 1)

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
splitWedgeHOdd w ls = Nil

-- | Divide a wedge into equal sub-spaces by radius
splitWedgeV :: Wedge -> Int -> List Space
splitWedgeV w i = Nil

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
