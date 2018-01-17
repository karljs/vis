module Canvas.Drawing
  ( convertRange
  , parseVis
  , splitBoxH
  , splitBoxV
  ) where

import Canvas.Types (CEffects, Rectangle(..), Space(..))
import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.List (List(..), zipWith, (:))
import Data.List.NonEmpty (length, toList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D, fillRect, setFillStyle, setLineWidth, setStrokeStyle, strokeRect)
import Prelude (bind, discard, pure, ($), (*), (+), (-), (/), (>=))
import V (Decision, Dir(..), lookupDim)
import Vis.Types (Frame(..), VVis(..))


parseVis :: forall m.
  Context2D ->
  Decision ->
  VVis Number ->
  Space ->
  Eff (CEffects m) Context2D
parseVis ctx dec (NextTo vs) (Cartesian r) = do
  let bs = splitBoxH r (length vs)
  sequence_ $ zipWith (parseVis ctx dec) (toList vs) bs
  pure ctx
parseVis ctx dec (Above vs) (Cartesian r) = do
  let bs = splitBoxV r (length vs)
  sequence_ $ zipWith (parseVis ctx dec) (toList vs) bs
  pure ctx
parseVis ctx dec (Fill v f) (Cartesian r) = do
  drawBox ctx v r f
parseVis ctx dec (V d l r) sp = do
  _ <- drawOutline ctx sp
  case lookupDim d dec of
    Just L -> parseVis ctx dec l sp
    Just R -> parseVis ctx dec r sp
    _      -> parseVis ctx dec l sp

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

drawBox :: forall m.
  Context2D -> Number -> Rectangle -> Frame Number -> Eff (CEffects m) Context2D
drawBox c v' (Rectangle r) (Frame f) = do
  let v = convertRange v' (Tuple f.frameMin f.frameMax) (Tuple (r.y + r.h) r.y)
      z = convertRange 0.0 (Tuple f.frameMin f.frameMax) (Tuple (r.y + r.h) r.y)

  _ <- setFillStyle "#657b83" c
  _ <- setStrokeStyle "#ffffff" c
  _ <- setLineWidth 1.0 c
  if v' >= 0.0
    then do
      _ <- fillRect c { x: r.x, y: v, w: r.w, h: z - v }
      strokeRect c { x: r.x, y: v, w: r.w, h: z - v }
    else do
      _ <- fillRect c { x: r.x , y: z , w: r.w , h: v - z }
      strokeRect c { x: r.x , y: z , w: r.w , h: v - z }

drawOutline :: forall m. Context2D -> Space -> Eff (CEffects m) Context2D
drawOutline ctx (Cartesian r) = drawOutlineRect ctx r

drawOutlineRect :: forall m.
  Context2D -> Rectangle -> Eff (CEffects m) Context2D
drawOutlineRect ctx (Rectangle r) = do
  _ <- setFillStyle "#dddddd" ctx
  _ <- setStrokeStyle "#ffffff" ctx
  _ <- setLineWidth 2.0 ctx
  _ <- fillRect ctx r
  strokeRect ctx r


convertRange :: Number -> Tuple Number Number -> Tuple Number Number -> Number
convertRange v (Tuple omin omax) (Tuple nmin nmax) =
  ((v - omin) / (omax - omin)) * (nmax - nmin) + nmin
