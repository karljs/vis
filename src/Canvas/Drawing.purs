module Canvas.Drawing
  ( convertRange
  , parseVis
  , splitBoxH
  , splitBoxV
  ) where

import Canvas.Types (CEffects, Space(..))
import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.List (List(..), zipWith, (:))
import Data.List.NonEmpty (length, toList)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D, fillRect, setFillStyle, setLineWidth, setStrokeStyle, strokeRect)
import Prelude (bind, discard, pure, ($), (*), (+), (-), (/), (>=))
import Vis.Types (Frame(..), Rectangle, VVis(..))


parseVis :: forall m.
  Context2D -> VVis Number -> Space -> Eff (CEffects m) Context2D
parseVis c (NextTo vs) (Box r) = do
  let bs = splitBoxH r (length vs)
  sequence_ $ zipWith (parseVis c) (toList vs) bs
  pure c
parseVis c (Above vs) (Box r) = do
  let bs = splitBoxV r (length vs)
  sequence_ $ zipWith (parseVis c) (toList vs) bs
  pure c
parseVis c (Fill v f) (Box r) = do
  drawBox c v r f
parseVis c (V d l r) s = parseVis c l s

splitBoxH :: Rectangle -> Int -> List Space
splitBoxH _ 0 = Nil
splitBoxH r i =
  let newW = r.w / toNumber i
  in Box (r { w = newW }) :
       splitBoxH (r { x = r.x + newW, w = r.w - newW }) (i - 1)

splitBoxV :: Rectangle -> Int -> List Space
splitBoxV _ 0 = Nil
splitBoxV r i =
  let newH = r.h / toNumber i
  in Box (r { h = newH }) :
       splitBoxV (r { y = r.y + newH, h = r.h - newH }) (i - 1)

drawBox :: forall m.
  Context2D -> Number -> Rectangle -> Frame Number -> Eff (CEffects m) Context2D
drawBox c v' r (Frame f) = do
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

convertRange :: Number -> Tuple Number Number -> Tuple Number Number -> Number
convertRange v (Tuple omin omax) (Tuple nmin nmax) =
  ((v - omin) / (omax - omin)) * (nmax - nmin) + nmin
