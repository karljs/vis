module Canvas.Drawing.Polar
  ( drawHintWedge
  , drawWedgeH
  , drawWedgeV
  ) where

import Canvas.Types (Wedge(..), CEffects)
import Color (Color, toHexString)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D, TextAlign(..), arc, beginPath, closePath, fill, fillText, lineTo, moveTo, setFillStyle, setFont, setLineDash, setLineWidth, setStrokeStyle, setTextAlign, stroke, strokePath, strokeText)
import Math (cos, sin)
import Prelude (Unit, discard, pure, show, unit, (*), (+), (/), (<>), (>=))
import Util (convertRange)
import Vis.Types (Frame(..), Label(..))

drawWedgeH :: forall m.
  Context2D ->
  Number ->
  Wedge ->
  Frame Number ->
  Maybe Label ->
  Eff (CEffects m) Unit
drawWedgeH ctx v (Wedge w) (Frame f) ml = do
  setFillStyle ctx "#657b83"
  setStrokeStyle ctx "#ffffff"
  setLineDash ctx []
  setLineWidth ctx 1.0
  fillWedge ctx (Wedge w)
  strokeWedge ctx (Wedge w)
  if v >= 0.0
    then maybe (pure unit) (\l -> drawLabelHP ctx l (Wedge w)) ml
    else maybe (pure unit) (\l -> drawLabelHN ctx l (Wedge w)) ml

drawWedgeV :: forall m.
  Context2D ->
  Number ->
  Wedge ->
  Frame Number ->
  Maybe Label ->
  Eff (CEffects m) Unit
drawWedgeV ctx v' (Wedge w) (Frame f) ml = do
  let v = convertRange v'  (Tuple f.frameMin f.frameMax)
                           (Tuple w.inRad w.outRad)
      z = convertRange 0.0 (Tuple f.frameMin f.frameMax)
                           (Tuple w.inRad w.outRad)
  setFillStyle ctx "#657b83"
  setStrokeStyle ctx "#ffffff"
  setLineDash ctx []
  setLineWidth ctx 1.0
  if v' >= 0.0
    then do let wedg = Wedge { x: w.x, y: w.y
                             , inRad: z , outRad: v
                             , startAngle: w.startAngle , endAngle: w.endAngle
                             }
            fillWedge ctx wedg
            strokeWedge ctx wedg
            maybe (pure unit) (\l -> drawLabelVP ctx l wedg) ml
    else do let wedg = Wedge { x: w.x, y: w.y
                             , inRad: v , outRad: z
                             , startAngle: w.startAngle , endAngle: w.endAngle
                             }
            fillWedge ctx wedg
            strokeWedge ctx wedg
            maybe (pure unit) (\l -> drawLabelVP ctx l wedg) ml

drawHintWedge :: forall m. Context2D -> Color -> Wedge -> Eff (CEffects m) Unit
drawHintWedge ctx col w = do
  setStrokeStyle ctx (toHexString col)
  setLineDash ctx [15.0, 5.0]
  setLineWidth ctx 4.0
  strokeWedge ctx w

--------------------------------------------------------------------------------
-- Wedge drawing functions that I wish were implemented for me

-- | Stroke the outline of a wedge shape, which is something like a piece of a
-- | doughnut.
strokeWedge :: forall m. Context2D -> Wedge -> Eff (CEffects m) Unit
strokeWedge ctx (Wedge w) = do
  beginPath ctx
  moveTo ctx ((cos w.startAngle) * w.inRad + w.x)
             ((sin w.startAngle) * w.inRad + w.y)
  lineTo ctx ((cos w.startAngle) * w.outRad + w.x)
             ((sin w.startAngle) * w.outRad + w.y)
  arc ctx { x: w.x, y: w.y
          , r: w.outRad
          , start: w.startAngle, end: w.endAngle
          , anti: false
          }
  lineTo ctx ((cos w.endAngle) * w.inRad + w.x)
             ((sin w.endAngle) * w.inRad + w.y)
  arc ctx { x: w.x, y: w.y
          , r: w.inRad
          , start: w.endAngle, end: w.startAngle
          , anti: true
          }
  stroke ctx

-- | Fill a wedge shape, which is something like a piece of a doughnut.
fillWedge :: forall m. Context2D -> Wedge -> Eff (CEffects m) Unit
fillWedge ctx (Wedge w) = do
  beginPath ctx
  moveTo ctx ((cos w.startAngle) * w.inRad + w.x)
             ((sin w.startAngle) * w.inRad + w.y)
  lineTo ctx ((cos w.startAngle) * w.outRad + w.x)
             ((sin w.startAngle) * w.outRad + w.y)
  arc ctx { x: w.x, y: w.y
          , r: w.outRad
          , start: w.startAngle, end: w.endAngle
          , anti: false
          }
  lineTo ctx ((cos w.endAngle) * w.inRad + w.x)
             ((sin w.endAngle) * w.inRad + w.y)
  arc ctx { x: w.x, y: w.y
          , r: w.inRad
          , start: w.endAngle, end: w.startAngle
          , anti: true
          }
  closePath ctx
  fill ctx

--------------------------------------------------------------------------------
-- Label drawing functions

  -- | Draw the label for a vertically oriented, positive valued wedge.
drawLabelVP :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelVP = drawLabelWedge

-- | Draw the label for a vertically oriented, negative valued bar.
drawLabelVN :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelVN = drawLabelWedge

-- | Draw the label for a horizontally oriented, positive valued bar.
drawLabelHP :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelHP = drawLabelWedge

-- | Draw the label for a horizontally oriented, negative valued bar.
drawLabelHN :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelHN = drawLabelWedge

-- | For now at least, all of the different wedges have their label position
-- | calculated the exact same way, so this function just factors it out.
drawLabelWedge :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelWedge ctx l (Wedge w) = do
  let ha = (w.startAngle + w.endAngle) / 2.0
      hr = (w.inRad + w.outRad) / 2.0
      tx = cos ha * hr + w.x
      ty = sin ha * hr + w.y
  drawLabelCommon ctx tx ty l

-- | Handle all of the label drawing parts that are common regardless of
-- | orientation, etc.
drawLabelCommon :: forall m.
  Context2D -> Number -> Number -> Label -> Eff (CEffects m) Unit
drawLabelCommon ctx tx ty (Label l) = do
  setFont ctx ("bold " <> show l.size <> "px sans-serif" )
  setFillStyle ctx "#ffffff"
  setStrokeStyle ctx "#000000"
  fillText ctx l.text tx ty
  strokeText ctx l.text tx ty