module Canvas
  ( cComponent
  ) where

import CSS (StyleM, backgroundColor, body, float, floatLeft, fontFamily, height, margin, nil, padding, pct, px, sansSerif, select, white, width)
import CSS.Overflow (hidden, overflow)
import Canvas.Drawing (parseVis)
import Canvas.Types (CEffects, CInput, CQuery(..), CState, Rectangle(..), Space(..), UISlot(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM.HTML (window)
import DOM.HTML.Window (innerHeight, innerWidth)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (singleton)
import Graphics.Canvas (CanvasElement, Context2D, clearRect, scale)
import Graphics.Canvas as C
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style, stylesheet)
import Halogen.HTML.Events (input)
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, bind, const, discard, id, pure, ($), (*), (-), (/))
import UI (uiComponent)
import UI.Types (UIQuery(..), UIMessage(..))
import V (Decision, emptyDec)
import Vis (VVis)

-- | The main canvas parent component which represents the bulk of the application.
cComponent :: forall m.
  H.Component HH.HTML CQuery (CInput Number) Void (Aff (CEffects m))
cComponent =
  H.lifecycleParentComponent
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Render)
    , finalizer: Nothing
    }
  where
  initialState :: VVis Number -> CState Number
  initialState v  =
    { currVis: v
    }

  render :: CState Number -> H.ParentHTML CQuery UIQuery UISlot (Aff (CEffects m))
  render state =
    HH.div
      [ HP.id_ "container"
      , style containerStyle
      ]
      [ stylesheet bodyStyle
      , HH.slot UISlot uiComponent (state.currVis) (input Changed)
      , HH.canvas
        [ HP.id_ "draw"
        , style canvasStyle
        ]
      ]

  eval ::
    CQuery ~>
      H.ParentDSL (CState Number) CQuery UIQuery UISlot Void (Aff (CEffects m))
  eval = case _ of
    Render next -> do
      mdec <- H.query UISlot $ H.request ViewDec
      mcan <- H.liftEff $ C.getCanvasElementById "draw"
      case mcan of
        Just can -> do
          s <- H.get
          _ <- H.liftEff (renderVisInit can (maybe emptyDec id mdec) s.currVis)
          pure next
        Nothing -> pure next
    Changed (Toggled dec) next -> do
      mcan <- H.liftEff $ C.getCanvasElementById "draw"
      case mcan of
        Just can -> do
          s <- H.get
          _ <- H.liftEff (renderVis can dec s.currVis)
          pure next
        Nothing -> pure next

-- | The entry point for rendering a visualization.  This function queries the
-- | view decision from the UI, performs a selection on the visualization, and
-- | passes that on into the drawing module.
renderVis :: forall m.
  CanvasElement ->
  Decision ->
  VVis Number ->
  Eff (CEffects m) Context2D
renderVis can dec vis = do
  w <- C.getCanvasWidth can
  h <- C.getCanvasHeight can
  ctx <- C.getContext2D can
  _ <- clearRect ctx { x: 0.0, y: 0.0, w: w / 2.0, h: h / 2.0}
  _ <- parseVis ctx dec vis
         (Cartesian (Rectangle { x: 0.0, y: 0.0, w: w / 2.0, h: h / 2.0}))
  pure ctx

-- | The same as renderVis, but do some setup things that should only occur
-- | once.
renderVisInit :: forall m.
  CanvasElement ->
  Decision ->
  VVis Number ->
  Eff (CEffects m) Context2D
renderVisInit can dec vis = do
  win <- window
  w <- innerWidth win
  h <- innerHeight win
  let h' = 2.0 * toNumber h - 20.0
      w' = 1.4 * toNumber w - 20.0
  _ <- C.setCanvasHeight (h' * 2.0) can
  _ <- C.setCanvasWidth (w' * 2.0) can
  ctx <- C.getContext2D can
  _ <- scale { scaleX: 2.0, scaleY: 2.0 } ctx
  _ <- parseVis ctx dec vis
         (Cartesian (Rectangle { x: 0.0, y: 0.0, w: w', h: h'}))
  pure ctx

--------------------------------------------------------------------------------
-- Style definitions

-- | The style for the actual HTML canvas we render to.
canvasStyle :: StyleM Unit
canvasStyle = do
  width (pct 68.0)
  height (pct 100.0)
  float floatLeft
  backgroundColor white
  padding (pct 1.0) (pct 1.0) (pct 1.0) (pct 1.0)

-- | The style for the outer UI container.
containerStyle :: StyleM Unit
containerStyle = do
  width (pct 100.0)
  height (pct 100.0)
  overflow hidden
  backgroundColor white
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
  fontFamily ["helvetica"] (singleton sansSerif)

bodyStyle :: StyleM Unit
bodyStyle = select body $ margin nil nil nil nil