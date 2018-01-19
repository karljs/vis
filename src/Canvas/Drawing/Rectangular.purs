module Canvas.Drawing.Rectangular
  ( drawBarH
  , drawBarV
  , drawHintRect
  )where

import Canvas.Types (CEffects, Rectangle(..))
import Color (Color, toHexString)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D, TextAlign(..), fillRect, fillText, setFillStyle, setFont, setLineDash, setLineWidth, setStrokeStyle, setTextAlign, strokeRect, strokeText)
import Prelude (Unit, discard, pure, show, unit, (+), (-), (/), (<>), (>=))
import Util (convertRange)
import Vis.Types (Frame(..), Label(..))

-- | Draw a vertically oriented rectangular bar.
drawBarV :: forall m.
  Context2D ->
  Number ->
  Rectangle ->
  Frame Number ->
  Maybe Label ->
  Eff (CEffects m) Unit
drawBarV ctx v' (Rectangle r) (Frame f) ml = do
  let v = convertRange v' (Tuple f.frameMin f.frameMax) (Tuple (r.y + r.h) r.y)
      z = convertRange 0.0 (Tuple f.frameMin f.frameMax) (Tuple (r.y + r.h) r.y)
  setFillStyle ctx "#657b83"
  setStrokeStyle ctx "#ffffff"
  setLineDash ctx []
  setLineWidth ctx 1.0
  if v' >= 0.0
    then do let rect = { x: r.x, y: v, w: r.w, h: z - v }
            fillRect ctx rect
            strokeRect ctx rect
            case ml of
              Just l -> drawLabelVP ctx l (Rectangle rect)
              Nothing -> pure unit
    else do let rect = { x: r.x , y: z , w: r.w , h: v - z }
            fillRect ctx rect
            strokeRect ctx rect
            case ml of
              Just l -> drawLabelVN ctx l (Rectangle rect)
              Nothing -> pure unit

-- | Draw a horizontally oriented rectangular bar.
drawBarH :: forall m.
  Context2D ->
  Number ->
  Rectangle ->
  Frame Number ->
  Maybe Label ->
  Eff (CEffects m) Unit
drawBarH ctx v' (Rectangle r) (Frame f) ml = do
  let v = convertRange v' (Tuple f.frameMin f.frameMax) (Tuple r.x (r.x + r.w))
      z = convertRange 0.0 (Tuple f.frameMin f.frameMax) (Tuple r.x (r.x + r.w))
  setFillStyle ctx "#657b83"
  setLineDash ctx []
  setStrokeStyle ctx "#ffffff"
  setLineWidth ctx 1.0
  if v' >= 0.0
    then do let rect = { x: z, y: r.y, w: v - z, h: r.h }
            fillRect ctx rect
            strokeRect ctx rect
            case ml of
              Just l -> drawLabelHP ctx l (Rectangle rect)
              Nothing -> pure unit
    else do let rect = { x: v, y: r.y, w: z - v, h: r.h }
            fillRect ctx rect
            strokeRect ctx rect
            case ml of
              Just l -> drawLabelHN ctx l (Rectangle rect)
              Nothing -> pure unit

-- | Draw the label for a vertically oriented, positive valued bar.
drawLabelVP :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabelVP ctx (Label l) (Rectangle r) = do
  setTextAlign ctx AlignCenter
  let tx = r.x + (r.w / 2.0)
      ty = r.y + l.labelSize
  drawLabelCommon ctx tx ty (Label l)

-- | Draw the label for a vertically oriented, negative valued bar.
drawLabelVN :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabelVN ctx (Label l) (Rectangle r) = do
  setTextAlign ctx AlignCenter
  let tx = r.x + (r.w / 2.0)
      ty = r.y + r.h - (l.labelSize / 1.8)
  drawLabelCommon ctx tx ty (Label l)

-- | Draw the label for a horizontally oriented, positive valued bar.
drawLabelHP :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabelHP ctx (Label l) (Rectangle r) = do
  setTextAlign ctx AlignRight
  let tx = r.x + r.w - (l.labelSize / 1.8)
      ty = r.y + ((r.h + l.labelSize) / 2.0)
  drawLabelCommon ctx tx ty (Label l)

-- | Draw the label for a horizontally oriented, negative valued bar.
drawLabelHN :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabelHN ctx (Label l) (Rectangle r) = do
  setTextAlign ctx AlignLeft
  let tx = r.x + (l.labelSize / 1.8)
      ty = r.y + ((r.h + l.labelSize) / 2.0)
  drawLabelCommon ctx tx ty (Label l)

-- | Handle all of the label drawing parts that are common regardless of
-- | orientation, etc.
drawLabelCommon :: forall m.
  Context2D -> Number -> Number -> Label -> Eff (CEffects m) Unit
drawLabelCommon ctx tx ty (Label l) = do
  setFont ctx ("bold " <> show l.labelSize <> "px sans-serif" )
  setFillStyle ctx "#ffffff"
  setStrokeStyle ctx "#000000"
  fillText ctx l.labelText tx ty
  strokeText ctx l.labelText tx ty

-- | Draw a hint suggesting variability in a rectangular space
drawHintRect :: forall m.
  Context2D -> Color -> Rectangle -> Eff (CEffects m) Unit
drawHintRect ctx col (Rectangle r) = do
  setStrokeStyle ctx (toHexString col)
  setLineDash ctx [15.0, 5.0]
  setLineWidth ctx 4.0
  strokeRect ctx r