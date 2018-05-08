module Canvas.Drawing
  ( parseVis
  , parseVisV

  , splitBoxH
  , splitBoxV
  , splitWedgeHEven
  , splitWedgeHOdd
  , splitWedgeVEven
  , splitWedgeVOdd

  , toCartesian
  , toPolar
  , toRectangle
  , toWedge
  ) where

import Canvas.Drawing.Polar (drawHintWedge, drawStackedWedges, drawWedge)
import Canvas.Drawing.Rectangular (drawBar, drawHintRect, drawSMRect, drawStackedBars)
import Canvas.Types (CEffects, Rectangle(..), Space(..), Wedge(..))
import Color (Color, black)
import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_, sum)
import Data.Int (toNumber)
import Data.List (List(..), filter, zipWith, (:))
import Data.List.NonEmpty (length, reverse, toList)
import Data.Map (lookup, singleton)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D)
import Math (pi)
import Prelude (Unit, discard, flip, map, min, pure, unit, ($), (*), (+), (-), (/))
import UI (DecisionColors)
import V (Decision, Dir(..), Dim, lookupDim, notInDec)
import Vis (Orientation(..), getColor, getHeight, getOrientation, getWidth, isVisible, removeCoord, selectVis, visDims)
import Vis.Types (VPs(..), VVis(..))

-- The main entry point for parsing apart a visualization and rendering it.
-- Before we can actually get to the work of doing this, we need to check
-- whether there are unselected dimensions, in which case we render small
-- multiples.
parseVisV :: forall m.
  Context2D ->
  Decision ->
  DecisionColors ->
  VVis ->
  Rectangle ->
  Eff (CEffects m) Unit
parseVisV ctx dec cs v r = do
  let vs = filter (flip notInDec dec) (visDims v)
  smallMult vs ctx dec cs v r

-- | Deal with small multiples before rendering individual visualization(s)
smallMult :: forall m.
  List Dim ->
  Context2D ->
  Decision ->
  DecisionColors ->
  VVis ->
  Rectangle ->
  Eff (CEffects m) Unit
smallMult Nil ctx dec cs v r = do
  parseVis ctx dec cs v (SpaceCartesian r)
  drawSMRect ctx r
smallMult (d : ds) ctx dec cs v r = do
  let (Tuple rl rr) = splitHalf r
  smallMult ds ctx dec cs (selectVis (singleton d L) v) rl
  smallMult ds ctx dec cs (selectVis (singleton d R) v) rr

-- | A helper function to split a rectangle in half.  Ideally this would be
-- | abstracted into the same function as the splitBox family, but we need a
-- | clever approach to avoid tons of annoying type unwrapping.
splitHalf :: Rectangle -> Tuple Rectangle Rectangle
splitHalf (Rectangle r) =
  Tuple (Rectangle { x: r.x              , y: r.y, w: r.w / 2.0, h: r.h } )
        (Rectangle { x: r.x + (r.w / 2.0), y: r.y, w: r.w / 2.0, h: r.h } )

-- | The starting point for rendering a single visualization (not small
-- | multiples) by parsing it and recursively diving up the space.
parseVis :: forall m.
  Context2D ->
  Decision ->
  DecisionColors ->
  VVis ->
  Space ->
  Eff (CEffects m) Unit
parseVis ctx dec cs (NextTo vs) (SpaceCartesian r) = do
  let bs = splitBoxH r (toList $ map (relativeWidth dec) vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) bs
parseVis ctx dec cs (NextTo vs) (SpacePolar w) = do
  let ws = splitWedgeHOdd w (toList $ map (relativeWidth dec) vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) ws

parseVis ctx dec cs (Above vs) (SpaceCartesian r) = do
  let bs = splitBoxV r (toList $ map (relativeHeight dec) vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) bs
parseVis ctx dec cs (Above vs) (SpacePolar w) = do
  let ws = splitWedgeVEven w (length vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) ws

parseVis ctx dec cs (Overlay vs) sp = do
  sequence_ $ map (\vis -> parseVis ctx dec cs vis sp) (reverse vs)

parseVis ctx dec cs (Stacked vs) (SpaceCartesian r) = do
  drawStackedBars ctx (toList vs) r
parseVis ctx dec cs (Stacked vs) (SpacePolar w) = do
  drawStackedWedges ctx (toList vs) w

parseVis ctx dec cs (V d l r) sp = do
  case lookupDim d dec of
    Just L -> parseVis ctx dec cs l sp
    Just R -> parseVis ctx dec cs r sp
    _      -> parseVis ctx dec cs l sp
  case lookup d cs of
    Just col -> drawVHint ctx col sp
    _        -> drawVHint ctx black sp

-- The removeCoord stuff shouldn't be here, but instead should be done when
-- applying the appropriate transformation functions.  This is a hack.
parseVis ctx dec cs (Cartesian v) s =
  parseVis ctx dec cs (removeCoord v) (toCartesian s)
parseVis ctx dec cs (Polar v) s =
  parseVis ctx dec cs (removeCoord v) (toPolar s)

parseVis ctx dec cs (Fill f) (SpaceCartesian r) =
  if isVisible f
  then drawBar ctx (getWidth f.vps) (getHeight f.vps) r f.frameW f.frameH
               f.label (getColor f.vps)
  else pure unit
parseVis ctx dec cs (Fill f) (SpacePolar w) =
  if isVisible f
  then drawWedge ctx (getWidth f.vps) (getHeight f.vps) w f.frameW f.frameH
                 f.label (getColor f.vps)
  else pure unit

-- | Draw some visual indicator that part of a chart contains variability.
drawVHint :: forall m. Context2D -> Color -> Space -> Eff (CEffects m) Unit
drawVHint ctx col (SpaceCartesian r) = drawHintRect ctx col r
drawVHint ctx col (SpacePolar w) = drawHintWedge ctx col w

-- | Determine the relative width a visualization component needs, which is
-- | useful when we are dividing up the space for rendering.
relativeWidth :: Decision -> VVis -> Number
relativeWidth _ (Fill f) =
  let (VPs vps) = f.vps
  in abs vps.width
relativeWidth dec (V d l r) =
  case lookupDim d dec of
    Just L -> relativeWidth dec l
    Just R -> relativeWidth dec r
    _      -> 1.0
relativeWidth _ (NextTo vs) = widthIfHorizontal (toList vs)
relativeWidth _ _ = 1.0

widthIfHorizontal :: List VVis -> Number
widthIfHorizontal Nil = 0.0
widthIfHorizontal (Fill f : vs) = case getOrientation f.vps of
  Vertical -> 1.0
  Horizontal -> getWidth f.vps + widthIfHorizontal vs
widthIfHorizontal _ = 1.0

relativeHeight :: Decision -> VVis -> Number
relativeHeight _ (Fill f) =
  let (VPs vps) = f.vps
  in abs vps.height
relativeHeight dec (V d l r) =
  case lookupDim d dec of
    Just L -> relativeHeight dec l
    Just R -> relativeHeight dec r
    _      -> 1.0
relativeHeight _ _ = 1.0

--------------------------------------------------------------------------------
-- Functions related to splitting spaces

-- | Divide a rectangular space into equal horizontal chunks.
splitBoxH :: Rectangle -> List Number -> List Space
splitBoxH r vs =
  splitBoxHT r r (map (\v -> v / (sum vs)) vs)

splitBoxHT :: Rectangle -> Rectangle -> List Number -> List Space
splitBoxHT _ _ Nil = Nil
splitBoxHT (Rectangle orig) (Rectangle r) (v : vs) =
  let newW = v * orig.w
  in SpaceCartesian (Rectangle (r { w = newW })) :
       splitBoxHT (Rectangle orig)
                  (Rectangle (r { x = r.x + newW, w = r.w - newW } ))
                  vs

splitBoxV :: Rectangle -> List Number -> List Space
splitBoxV r vs =
  splitBoxVT r r (map (\v -> v / (sum vs)) vs)

splitBoxVT :: Rectangle -> Rectangle -> List Number -> List Space
splitBoxVT _ _ Nil = Nil
splitBoxVT (Rectangle orig) (Rectangle r) (v : vs) =
  let newH = v * orig.h
  in SpaceCartesian (Rectangle (r { h = newH })) :
       splitBoxVT (Rectangle orig)
                  (Rectangle (r { y = r.y + newH, h = r.h - newH } ))
                  vs

-- | Divide a wedge into equal sub-spaces by angle such as for a coxcomb
-- | chart.
splitWedgeHEven :: Wedge -> Int -> List Space
splitWedgeHEven _ 0 = Nil
splitWedgeHEven (Wedge w) i =
  let newA = ((w.endAngle - w.startAngle) / toNumber i) + w.startAngle
  in SpacePolar (Wedge (w { endAngle = newA })) :
       splitWedgeHEven (Wedge (w { startAngle = newA })) (i - 1)

-- | Divide a wedge into unequal sub-spaces by angle such as for a pie chart.
splitWedgeHOdd :: Wedge -> List Number -> List Space
splitWedgeHOdd w ls = split w ls (sum ls)
  where split :: Wedge -> List Number -> Number -> List Space
        split _ Nil _ = Nil
        split (Wedge w) (v : vs) t =
          let newAngle = (v / t) * (w.endAngle - w.startAngle) + w.startAngle
          in SpacePolar (Wedge (w { endAngle = newAngle })) :
               split (Wedge (w { startAngle = newAngle })) vs (t - v)

-- | Divide a wedge into equal sub-spaces by radius
splitWedgeVEven :: Wedge -> Int -> List Space
splitWedgeVEven _ 0 = Nil
splitWedgeVEven (Wedge w) i =
  let newR = ((w.outRad - w.inRad) / toNumber i) + w.inRad
  in SpacePolar (Wedge (w { outRad = newR })) :
       splitWedgeVEven (Wedge (w { inRad = newR })) (i - 1)

splitWedgeVOdd :: Wedge -> List Number -> List Space
splitWedgeVOdd w ls = Nil

--------------------------------------------------------------------------------
-- Functions for converting between polar and Cartesian coordinate systems

-- | Convert some chunk of space to a Cartesian coordinate system
toCartesian :: Space -> Space
toCartesian (SpacePolar w) = SpaceCartesian (toRectangle w)
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
toPolar (SpaceCartesian r) = SpacePolar (toWedge r)
toPolar s = s

toWedge :: Rectangle -> Wedge
toWedge (Rectangle r) = Wedge { x: r.x + (r.w / 2.0)
                              , y: r.y + (r.h / 2.0)
                              , inRad: 0.0
                              , outRad: (min r.h r.w) / 2.0
                              , startAngle: 0.0
                              , endAngle: 2.0 * pi
                              }
