module Canvas.Drawing
  ( parseVis
  , splitBoxH
  ) where

import Canvas.Types (CEffects, Space (..))
import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.List (List(..), zipWith, (:))
import Data.List.NonEmpty (length, toList)
import Graphics.Canvas (Context2D, fillRect, setFillStyle, setLineWidth, setStrokeStyle, strokeRect)
import Prelude (bind, discard, pure, (+), (-), (*), (/), ($))
import Vis (Rectangle, VVis(..))


parseVis :: forall m.
  Context2D -> VVis Number -> Space -> Eff (CEffects m) Context2D
parseVis c (NextTo vs) (Box r) = do
  let bs = splitBoxH r (length vs)
  sequence_ $ zipWith (parseVis c) (toList vs) bs
  pure c
parseVis c (Fill v m) (Box r) = do
  drawBox c (v / m) r
parseVis c (V d l r) s = parseVis c l s

splitBoxH :: Rectangle -> Int -> List Space
splitBoxH _ 0 = Nil
splitBoxH r i =
  let newW = r.w / toNumber i
  in Box (r { w = newW }) :
       splitBoxH (r { x = r.x + newW, w = r.w - newW }) (i - 1)

drawBox :: forall m.
  Context2D -> Number -> Rectangle -> Eff (CEffects m) Context2D
drawBox c v r = do
  _ <- setFillStyle "#657b83" c
  _ <- fillRect c { x: r.x, y: r.h - (v * r.h), w: r.w, h: v * r.h }
  _ <- setStrokeStyle "#ffffff" c
  _ <- setLineWidth 1.0 c
  strokeRect c { x: r.x, y: r.h - (v * r.h), w: r.w, h: v * r.h }
