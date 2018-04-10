module Canvas.Drawing.Rectangular
  ( drawBar
  , drawHintRect
  , drawSMRect
  , drawStackedBars
  )where

import Canvas.Types (CEffects, Rectangle(..))
import Color (Color, black, cssStringRGBA, white)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Data.List.Types (List, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Canvas (Context2D, TextAlign(..), fillRect, fillText, setFillStyle, setFont, setLineDash, setLineWidth, setStrokeStyle, setTextAlign, strokeRect, strokeText)
import Prelude (Unit, discard, pure, show, unit, ($), (&&), (+), (-), (/), (<), (<>), (>=))
import Util (convertRange)
import Vis (Orientation(..), VPs(..), VVis(..), getColor, getHeight, getOrientation, getWidth, maybe1, splitPosNeg)
import Vis.Types (Frame(..), Label(..), Orientation(..), VVis)

drawBar :: forall m.
  Context2D ->
  Number ->
  Number ->
  Rectangle ->
  Frame Number ->
  Frame Number ->
  Maybe Label ->
  Color ->
  Eff (CEffects m) Unit
drawBar ctx w h (Rectangle r) (Frame fw) (Frame fh) ml col = do
  let vh = convertRange h   (Tuple fh.frameMin fh.frameMax)
                            (Tuple (r.y + r.h) r.y)
      zh = convertRange 0.0 (Tuple fh.frameMin fh.frameMax)
                            (Tuple (r.y + r.h) r.y)
      vw = convertRange w   (Tuple fw.frameMin fw.frameMax)
                            (Tuple r.x (r.x + r.w))
      zw = convertRange 0.0 (Tuple fw.frameMin fw.frameMax)
                            (Tuple r.x (r.x + r.w))
  setFillStyle ctx (cssStringRGBA col)
  setStrokeStyle ctx "#ffffff"
  setLineDash ctx []
  setLineWidth ctx 1.0
  let rect = if h >= 0.0 && w >= 0.0
             then { x: zw, y: vh, w: vw - zw, h: zh - vh }
             else if h < 0.0
                  then { x: zw , y: zh , w: vw - zw , h: vh - zh }
                  else { x: vw , y: vh , w: zw - vw , h: zh - vh }
  fillRect ctx rect
  strokeRect ctx rect
  case ml of
          Just l -> drawLabel ctx l (Rectangle rect)
          Nothing -> pure unit

drawStackedBars :: forall m.
  Context2D ->
  List (VVis Number) ->
  Rectangle ->
  Eff (CEffects m) Unit
drawStackedBars ctx ((Fill v) : vs) (Rectangle r) = do
  let (Tuple pos neg) = splitPosNeg (Fill v : vs)
      (Frame fh) = v.frameH
      (Frame fw) = v.frameW
      zh = convertRange 0.0 (Tuple fh.frameMin fh.frameMax)
                            (Tuple (r.y + r.h) r.y)
      zw = convertRange 0.0 (Tuple fw.frameMin fw.frameMax)
                            (Tuple r.x (r.x + r.w))
  setLineDash ctx []
  setLineWidth ctx 1.0
  drawStackedPos ctx pos (Rectangle r) zh zw 0.0 0.0
  drawStackedNeg ctx neg (Rectangle r) zh zw 0.0 0.0
drawStackedBars _ _ _ = pure unit

drawStackedPos :: forall m.
  Context2D ->
  List (VVis Number) ->
  Rectangle ->
  Number -> Number ->
  Number -> Number ->
  Eff (CEffects m) Unit
drawStackedPos ctx (Fill v : vs) (Rectangle r) zh zw oh ow = do
  let (Frame fh) = v.frameH
      (Frame fw) = v.frameW
      (VPs vps) = v.vps
      h = maybe1 vps.height
      w = maybe1 vps.width
      vh = convertRange h (Tuple fh.frameMin fh.frameMax)
                          (Tuple (r.y + r.h) r.y)
      vw = convertRange w (Tuple fw.frameMin fw.frameMax)
                          (Tuple r.x (r.x + r.w))
      brect = { x: zw + ow, y: vh - oh, w: vw - zw, h: zh - vh }
  setFillStyle ctx (cssStringRGBA (vps.color))
  setStrokeStyle ctx "#ffffff"
  fillRect ctx brect
  strokeRect ctx brect
  case v.label of
          Just l -> drawLabel ctx l (Rectangle brect)
          Nothing -> pure unit
  case vps.orientation of
    Vertical   -> drawStackedPos ctx vs (Rectangle r) zh zw (oh + (zh - vh)) ow
    Horizontal -> drawStackedPos ctx vs (Rectangle r) zh zw oh (ow + (vw - zw))
drawStackedPos _ _ _ _ _ _ _ = pure unit

drawStackedNeg :: forall m.
  Context2D ->
  List (VVis Number) ->
  Rectangle ->
  Number -> Number ->
  Number -> Number ->
  Eff (CEffects m) Unit
drawStackedNeg ctx (Fill v : vs) (Rectangle r) zh zw oh ow = do
  let (Frame fh) = v.frameH
      (Frame fw) = v.frameW
      h = getHeight v.vps
      w = getWidth v.vps
      vh = convertRange h (Tuple fh.frameMin fh.frameMax)
                          (Tuple (r.y + r.h) r.y)
      vw = convertRange w (Tuple fw.frameMin fw.frameMax)
                          (Tuple r.x (r.x + r.w))
      brect = case getOrientation v.vps of
                Vertical -> { x: zw , y: zh + oh, w: vw - zw , h: vh - zh }
                Horizontal -> { x: vw + ow, y: vh , w: zw - vw , h: zh - vh }
  setFillStyle ctx (cssStringRGBA (getColor v.vps))
  setStrokeStyle ctx "#ffffff"
  fillRect ctx brect
  strokeRect ctx brect
  case v.label of
          Just l -> drawLabel ctx l (Rectangle brect)
          Nothing -> pure unit
  case getOrientation v.vps of
    Vertical -> drawStackedNeg ctx vs (Rectangle r) zh zw (oh + (vh - zh)) ow
    Horizontal -> drawStackedNeg ctx vs (Rectangle r) zh zw oh (ow + (zw - vw))
drawStackedNeg _ _ _ _ _ _ _ = pure unit

-- | Draw an outline for the area dedicated to a small multiple, to help set it
-- | apart.
drawSMRect :: forall m. Context2D -> Rectangle -> Eff (CEffects m) Unit
drawSMRect ctx (Rectangle r) = do
  setFillStyle ctx (cssStringRGBA white)
  setStrokeStyle ctx (cssStringRGBA black)
  setLineDash ctx []
  setLineWidth ctx 1.0
  strokeRect ctx r

--------------------------------------------------------------------------------
-- Functions for drawing labels

-- | Draw a centered label, because dealing with the positioning after removing
-- | explicit orientation is out of scope for now.
drawLabel :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabel ctx l (Rectangle r) = do
  setTextAlign ctx AlignCenter
  let tx = r.x + (r.w / 2.0)
      ty = r.y + (r.h / 2.0)
  drawLabelCommon ctx tx ty l

-- | Draw the label for a vertically oriented, positive valued bar.
drawLabelVP :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabelVP ctx (Label l) (Rectangle r) = do
  setTextAlign ctx AlignCenter
  let tx = r.x + (r.w / 2.0)
      ty = r.y + l.size
  drawLabelCommon ctx tx ty (Label l)

-- | Draw the label for a vertically oriented, negative valued bar.
drawLabelVN :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabelVN ctx (Label l) (Rectangle r) = do
  setTextAlign ctx AlignCenter
  let tx = r.x + (r.w / 2.0)
      ty = r.y + r.h - (l.size / 1.8)
  drawLabelCommon ctx tx ty (Label l)

-- | Draw the label for a horizontally oriented, positive valued bar.
drawLabelHP :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabelHP ctx (Label l) (Rectangle r) = do
  setTextAlign ctx AlignRight
  let tx = r.x + r.w - (l.size / 1.8)
      ty = r.y + ((r.h + l.size) / 2.0)
  drawLabelCommon ctx tx ty (Label l)

-- | Draw the label for a horizontally oriented, negative valued bar.
drawLabelHN :: forall m.
  Context2D -> Label -> Rectangle -> Eff (CEffects m) Unit
drawLabelHN ctx (Label l) (Rectangle r) = do
  setTextAlign ctx AlignLeft
  let tx = r.x + (l.size / 1.8)
      ty = r.y + ((r.h + l.size) / 2.0)
  drawLabelCommon ctx tx ty (Label l)

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

-- | Draw a hint suggesting variability in a rectangular space
drawHintRect :: forall m.
  Context2D -> Color -> Rectangle -> Eff (CEffects m) Unit
drawHintRect ctx col (Rectangle r) = do
  setStrokeStyle ctx (cssStringRGBA col)
  setLineDash ctx [15.0, 5.0]
  setLineWidth ctx 4.0
  strokeRect ctx r