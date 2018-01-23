module Canvas.Drawing.Polar
  ( drawHintWedge
  ) where

import Canvas.Types (Wedge(..), CEffects)
import Color (Color, toHexString)
import Control.Monad.Eff (Eff)
import Graphics.Canvas (Context2D, arc, beginPath, lineTo, moveTo, setLineDash, setLineWidth, setStrokeStyle, stroke)
import Math (cos, sin)
import Prelude (Unit, discard, (*), (+))

drawHintWedge :: forall m. Context2D -> Color -> Wedge -> Eff (CEffects m) Unit
drawHintWedge ctx col w = do
  setStrokeStyle ctx (toHexString col)
  setLineDash ctx [15.0, 5.0]
  setLineWidth ctx 4.0
  -- strokeRect ctx { x:0.0, y:0.0, w:20.0, h:20.0 }
  strokeWedge ctx w

--------------------------------------------------------------------------------
-- Wedge drawing functions that I wish were implemented for me

strokeWedge :: forall m. Context2D -> Wedge -> Eff (CEffects m) Unit
strokeWedge ctx (Wedge w) = do
  let p1x = (cos w.startAngle) * w.inRad + w.x
      p1y = (sin w.startAngle) * w.inRad + w.y
      p2x = (cos w.startAngle) * w.outRad + w.x
      p2y = (sin w.startAngle) * w.outRad + w.y
      p3x = (cos w.endAngle) * w.outRad + w.x
      p3y = (sin w.endAngle) * w.outRad + w.y
      p4x = (cos w.endAngle) * w.inRad + w.x
      p4y = (sin w.endAngle) * w.inRad + w.y
  beginPath ctx
  moveTo ctx p1x p1y
  lineTo ctx p2x p2y
  arc ctx { x: w.x, y: w.y
          , r: w.outRad
          , start: w.startAngle, end: w.endAngle
          , anti: false
          }
  lineTo ctx p4x p4y
  arc ctx { x: w.x, y: w.y
          , r: w.inRad
          , start: w.endAngle, end: w.startAngle
          , anti: true
          }
  stroke ctx