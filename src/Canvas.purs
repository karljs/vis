module Canvas
  ( cComponent
  ) where

-- import CSS (StyleM, backgroundColor, float, floatLeft, fontFamily, height, margin, pct, px, sansSerif, white, width)
-- import CSS.Overflow (hidden, overflow)
import Canvas.Drawing (parseVis)
import Canvas.Types (CEffects, CInput, CQuery(..), CState, Space(..), UISlot(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CanvasElement, Context2D, clearRect, translate)
import Graphics.Canvas as C
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (input)
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Void, bind, const, negate, pure, ($))
import UI (uiComponent)
import UI.Types (UIQuery(..), UIMessage(..))
import Vis (VVis, selectVis, selectVisM)
-- import VisColor (background)

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
      ]
      [ HH.slot UISlot uiComponent (state.currVis) (input Changed)
      , HH.canvas
        [ HP.id_ "draw"
        -- , style canvasStyle
        ]
      ]

  eval ::
    CQuery ~>
      H.ParentDSL (CState Number) CQuery UIQuery UISlot Void (Aff (CEffects m))
  eval = case _ of
    Render next -> do
      dec <- H.query UISlot $ H.request ViewDec
      mc <- H.liftEff $ C.getCanvasElementById "draw"
      case mc of
        Just c -> do
          s <- H.get
          _ <- H.liftEff (renderVisInit c (selectVisM dec s.currVis))
          pure next
        Nothing -> pure next
    Changed (Toggled dec) next -> do
      mc <- H.liftEff $ C.getCanvasElementById "draw"
      case mc of
        Just c -> do
          s <- H.get
          _ <- H.liftEff (renderVis c (selectVis dec s.currVis))
          pure next
        Nothing -> pure next

-- | The entry point for rendering a visualization.  This function queries the
-- | view decision from the UI, performs a selection on the visualization, and
-- | passes that on into the drawing module.
renderVis :: forall m.
  CanvasElement -> VVis Number -> Eff (CEffects m) Context2D
renderVis can v = do
  w <- C.getCanvasWidth can
  h <- C.getCanvasHeight can
  c <- C.getContext2D can
  _ <- clearRect c { x: 0.0, y: 0.0, w: w, h: h}
  _ <- parseVis c v (Box { x: 0.0, y: 0.0, w: w, h: h})
  pure c

-- | The same as renderVis, but do some setup things that should only occur
-- | once.
renderVisInit :: forall m.
  CanvasElement -> VVis Number -> Eff (CEffects m) Context2D
renderVisInit can v = do
  w <- C.getCanvasWidth can
  h <- C.getCanvasHeight can
  c <- C.getContext2D can
  -- _ <- translate {translateX: (0.5), translateY: (0.5) } c
  _ <- parseVis c v (Box { x: 0.0, y: 0.0, w: w, h: h})
  pure c

--------------------------------------------------------------------------------
-- Style definitions

-- | The style for the actual HTML canvas we render to.
-- canvasStyle :: StyleM Unit
-- canvasStyle = do
--   width (pct 69.0)
--   height (pct 100.0)
--   float floatLeft
--   backgroundColor white

-- | The style for the outer UI container.
-- containerStyle :: StyleM Unit
-- containerStyle = do
--   width (pct 100.0)
--   height (pct 100.0)
--   overflow hidden
--   backgroundColor background
--   margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
--   fontFamily ["helvetica"] (singleton sansSerif)
