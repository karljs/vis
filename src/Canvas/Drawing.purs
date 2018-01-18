module Canvas.Drawing
  ( convertRange
  , parseVis
  , splitBoxH
  , splitBoxV
  ) where

import Canvas.Types (CEffects, Rectangle(..), Space(..))
import Color (Color, black, toHexString)
import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.List (List(..), zipWith, (:))
import Data.List.NonEmpty (length, toList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D, fillRect, setFillStyle, setLineDash, setLineWidth, setStrokeStyle, strokeRect)
import Prelude (Unit, bind, ($), (*), (+), (-), (/), (>=))
import UI (DecisionColors)
import V (Decision, Dir(..), lookupDim)
import Vis.Types (Frame(..), Orientation(..), VVis(..))

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
  -- pure ctx
parseVis ctx dec cs (Above vs) (Cartesian r) = do
  let bs = splitBoxV r (length vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) bs
  -- pure ctx
parseVis ctx dec cs (Fill v f OrientVertical) (Cartesian r) = do
  drawBoxV ctx v r f
parseVis ctx dec cs (Fill v f OrientHorizontal) (Cartesian r) = do
  drawBoxH ctx v r f
parseVis ctx dec cs (V d l r) sp = do
  _ <- case lookup d cs of
         Just col -> drawVHint ctx col sp
         _ -> drawVHint ctx black sp
  case lookupDim d dec of
    Just L -> parseVis ctx dec cs l sp
    Just R -> parseVis ctx dec cs r sp
    _      -> parseVis ctx dec cs l sp

splitBoxH :: Rectangle -> Int -> List Space
splitBoxH _ 0 = Nil
splitBoxH (Rectangle r) i =
  let newW = r.w / toNumber i
  in Cartesian (Rectangle (r { w = newW })) :
       splitBoxH (Rectangle (r { x = r.x + newW, w = r.w - newW })) (i - 1)

splitBoxV :: Rectangle -> Int -> List Space
splitBoxV _ 0 = Nil
splitBoxV (Rectangle r) i =
  let newH = r.h / toNumber i
  in Cartesian (Rectangle (r { h = newH })) :
       splitBoxV (Rectangle (r { y = r.y + newH, h = r.h - newH })) (i - 1)

drawBoxV :: forall m.
  Context2D -> Number -> Rectangle -> Frame Number -> Eff (CEffects m) Unit
drawBoxV ctx v' (Rectangle r) (Frame f) = do
  let v = convertRange v' (Tuple f.frameMin f.frameMax) (Tuple (r.y + r.h) r.y)
      z = convertRange 0.0 (Tuple f.frameMin f.frameMax) (Tuple (r.y + r.h) r.y)
  _ <- setFillStyle ctx "#657b83"
  _ <- setStrokeStyle ctx "#ffffff"
  _ <- setLineWidth ctx 1.0
  if v' >= 0.0
    then do
      _ <- fillRect ctx { x: r.x, y: v, w: r.w, h: z - v }
      strokeRect ctx { x: r.x, y: v, w: r.w, h: z - v }
    else do
      _ <- fillRect ctx { x: r.x , y: z , w: r.w , h: v - z }
      strokeRect ctx { x: r.x , y: z , w: r.w , h: v - z }

drawBoxH :: forall m.
  Context2D -> Number -> Rectangle -> Frame Number -> Eff (CEffects m) Unit
drawBoxH ctx v' (Rectangle r) (Frame f) = do
  let v = convertRange v' (Tuple f.frameMin f.frameMax) (Tuple r.x (r.x + r.w))
      z = convertRange 0.0 (Tuple f.frameMin f.frameMax) (Tuple r.x (r.x + r.w))
  _ <- setFillStyle ctx "#657b83"
  _ <- setStrokeStyle ctx "#ffffff"
  _ <- setLineWidth ctx 1.0
  if v' >= 0.0
    then do
      _ <- fillRect ctx { x: z, y: r.y, w: v - z, h: r.h }
      strokeRect ctx { x: z, y: r.y, w: v - z, h: r.h }
    else do
      _ <- fillRect ctx { x: v, y: r.y, w: z - v, h: r.h }
      strokeRect ctx { x: v, y: r.y, w: z - v, h: r.h }

drawVHint :: forall m. Context2D -> Color -> Space -> Eff (CEffects m) Unit
drawVHint ctx col (Cartesian r) = drawHintRect ctx col r

drawHintRect :: forall m.
  Context2D -> Color -> Rectangle -> Eff (CEffects m) Unit
drawHintRect ctx col (Rectangle r) = do
  _ <- setStrokeStyle ctx (toHexString col)
  _ <- setLineDash ctx [15.0, 5.0]
  _ <- setLineWidth ctx 4.0
  strokeRect ctx r


convertRange :: Number -> Tuple Number Number -> Tuple Number Number -> Number
convertRange v (Tuple omin omax) (Tuple nmin nmax) =
  ((v - omin) / (omax - omin)) * (nmax - nmin) + nmin
