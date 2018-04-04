module Canvas.Drawing.Polar
  ( drawHintWedge
  , drawWedge
  ) where

import Canvas.Types (Wedge(..), CEffects)
import Color (Color, cssStringRGBA)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D, TextAlign(..), arc, beginPath, closePath, fill, fillText, lineTo, moveTo, setFillStyle, setFont, setLineDash, setLineWidth, setStrokeStyle, setTextAlign, stroke, strokeText)
import Math (cos, sin)
import Prelude (Unit, discard, pure, show, unit, (*), (+), (/), (<>), (>=))
import Util (convertRange)
import Vis.Types (Frame(..), Label(..))

drawWedge :: forall m.
  Context2D ->
  Number ->
  Number ->
  Wedge ->
  Frame Number ->
  Frame Number ->
  Maybe Label ->
  Color ->
  Eff (CEffects m) Unit
drawWedge ctx w h (Wedge wedg') (Frame fw) (Frame fh) ml col = do
  let vh = convertRange h   (Tuple fh.frameMin fh.frameMax)
                            (Tuple wedg'.inRad wedg'.outRad)
      zh = convertRange 0.0 (Tuple fh.frameMin fh.frameMax)
                            (Tuple wedg'.inRad wedg'.outRad)
  setFillStyle ctx (cssStringRGBA col)
  setStrokeStyle ctx "#ffffff"
  setLineDash ctx []
  setLineWidth ctx 1.0
  let wedg = if h >= 0.0
             then Wedge { x: wedg'.x, y: wedg'.y
                        , inRad: zh , outRad: vh
                        , startAngle: wedg'.startAngle
                        , endAngle: wedg'.endAngle
                        }
             else Wedge { x: wedg'.x, y: wedg'.y
                        , inRad: vh , outRad: zh
                        , startAngle: wedg'.startAngle
                        , endAngle: wedg'.endAngle
                        }
  fillWedge ctx wedg
  strokeWedge ctx wedg
  maybe (pure unit) (\l -> drawLabelVP ctx l wedg) ml

drawHintWedge :: forall m. Context2D -> Color -> Wedge -> Eff (CEffects m) Unit
drawHintWedge ctx col w = do
  setStrokeStyle ctx (cssStringRGBA col)
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
  setTextAlign ctx AlignCenter
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