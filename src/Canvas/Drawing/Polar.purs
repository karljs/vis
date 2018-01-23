module Canvas.Drawing.Polar
  ( drawHintWedge
  , drawWedgeV
  ) where

import Canvas.Types (Wedge(..), CEffects)
import Color (Color, toHexString)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D, TextAlign(..), arc, beginPath, closePath, fill, fillText, lineTo, moveTo, setFillStyle, setFont, setLineDash, setLineWidth, setStrokeStyle, setTextAlign, stroke, strokePath, strokeText)
import Math (cos, sin)
import Prelude (Unit, discard, pure, unit, (*), (+), (<>), (>=))
import Util (convertRange)
import Vis.Types (Frame(..), Label(..))

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
            case ml of
              Just l -> pure unit
              Nothing -> pure unit
    else do let wedg = Wedge { x: w.x, y: w.y
                             , inRad: v , outRad: z
                             , startAngle: w.startAngle , endAngle: w.endAngle
                             }
            fillWedge ctx wedg
            strokeWedge ctx wedg
            case ml of
              Just l -> pure unit
              Nothing -> pure unit

drawHintWedge :: forall m. Context2D -> Color -> Wedge -> Eff (CEffects m) Unit
drawHintWedge ctx col w = do
  setStrokeStyle ctx (toHexString col)
  setLineDash ctx [15.0, 5.0]
  setLineWidth ctx 4.0
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

fillWedge :: forall m. Context2D -> Wedge -> Eff (CEffects m) Unit
fillWedge ctx (Wedge w) = do
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
  closePath ctx
  fill ctx

--------------------------------------------------------------------------------
-- Label drawing functions

  -- | Draw the label for a vertically oriented, positive valued wedge.
drawLabelVP :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelVP ctx (Label l) (Wedge w) = do
  setTextAlign ctx AlignCenter
  -- let tx = r.x + (r.w / 2.0)
  --     ty = r.y + l.labelSize
  -- drawLabelCommon ctx tx ty (Label l)
  pure unit

-- | Draw the label for a vertically oriented, negative valued bar.
drawLabelVN :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelVN ctx (Label l) (Wedge w) = do
  setTextAlign ctx AlignCenter
  -- let tx = r.x + (r.w / 2.0)
  --     ty = r.y + r.h - (l.labelSize / 1.8)
  -- drawLabelCommon ctx tx ty (Label l)
  pure unit

-- | Draw the label for a horizontally oriented, positive valued bar.
drawLabelHP :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelHP ctx (Label l) (Wedge w) = do
  -- setTextAlign ctx AlignRight
  -- let tx = r.x + r.w - (l.labelSize / 1.8)
  --     ty = r.y + ((r.h + l.labelSize) / 2.0)
  -- drawLabelCommon ctx tx ty (Label l)
  pure unit

-- | Draw the label for a horizontally oriented, negative valued bar.
drawLabelHN :: forall m.
  Context2D -> Label -> Wedge -> Eff (CEffects m) Unit
drawLabelHN ctx (Label l) (Wedge w) = do
  -- setTextAlign ctx AlignLeft
  -- let tx = r.x + (l.labelSize / 1.8)
  --     ty = r.y + ((r.h + l.labelSize) / 2.0)
  -- drawLabelCommon ctx tx ty (Label l)
  pure unit

-- | Handle all of the label drawing parts that are common regardless of
-- | orientation, etc.
drawLabelCommon :: forall m.
  Context2D -> Number -> Number -> Label -> Eff (CEffects m) Unit
drawLabelCommon ctx tx ty (Label l) = do
  pure unit
  -- setFont ctx ("bold " <> show l.labelSize <> "px sans-serif" )
  -- setFillStyle ctx "#ffffff"
  -- setStrokeStyle ctx "#000000"
  -- fillText ctx l.labelText tx ty
  -- strokeText ctx l.labelText tx ty