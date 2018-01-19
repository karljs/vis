module Canvas.Drawing
  ( parseVis

  , splitBoxH
  , splitBoxV
  ) where

import Canvas.Drawing.Rectangular (drawBarH, drawBarV, drawHintRect)
import Canvas.Types (CEffects, Rectangle(..), Space(..))
import Color (Color, black)
import Control.Monad.Eff (Eff)
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.List (List(..), zipWith, (:))
import Data.List.NonEmpty (length, toList)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (Context2D)
import Prelude (Unit, bind, ($), (+), (-), (/))
import UI (DecisionColors)
import V (Decision, Dir(..), lookupDim)
import Vis.Types (Orientation(..), VVis(..))

-- | The main entry point for rendering a visualization by parsing it and
-- | recursively diving up the space.
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
parseVis ctx dec cs (Above vs) (Cartesian r) = do
  let bs = splitBoxV r (length vs)
  sequence_ $ zipWith (parseVis ctx dec cs) (toList vs) bs
parseVis ctx dec cs (V d l r) sp = do
  _ <- case lookup d cs of
         Just col -> drawVHint ctx col sp
         _ -> drawVHint ctx black sp
  case lookupDim d dec of
    Just L -> parseVis ctx dec cs l sp
    Just R -> parseVis ctx dec cs r sp
    _      -> parseVis ctx dec cs l sp
parseVis ctx dec cs (Fill f) (Cartesian r) =
  case f.fillOrientation of
    OrientVertical -> drawBarV ctx f.fillVal r f.fillFrame f.fillLabel
    OrientHorizontal -> drawBarH ctx f.fillVal r f.fillFrame f.fillLabel


-- | Divide a rectangular space into equal horizontal chunks.
splitBoxH :: Rectangle -> Int -> List Space
splitBoxH _ 0 = Nil
splitBoxH (Rectangle r) i =
  let newW = r.w / toNumber i
  in Cartesian (Rectangle (r { w = newW })) :
       splitBoxH (Rectangle (r { x = r.x + newW, w = r.w - newW })) (i - 1)

-- | Divide a rectangular space into equal vertical chunks.
splitBoxV :: Rectangle -> Int -> List Space
splitBoxV _ 0 = Nil
splitBoxV (Rectangle r) i =
  let newH = r.h / toNumber i
  in Cartesian (Rectangle (r { h = newH })) :
       splitBoxV (Rectangle (r { y = r.y + newH, h = r.h - newH })) (i - 1)

drawVHint :: forall m. Context2D -> Color -> Space -> Eff (CEffects m) Unit
drawVHint ctx col (Cartesian r) = drawHintRect ctx col r
