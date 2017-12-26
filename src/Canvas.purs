module Canvas
  ( cComponent
  ) where

import CSS (StyleM, backgroundColor, float, floatLeft, fontFamily, height, margin, pct, px, sansSerif, white, width)
import CSS.Overflow (hidden, overflow)
import Canvas.Drawing (parseVis)
import Canvas.Types (CEffects, CInput, CQuery (..), CState, Space (..), UISlot (..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (singleton)
import Graphics.Canvas (CanvasElement, Context2D, translate)
import Graphics.Canvas as C
import Halogen as H
import Halogen.HTML.Events (input)
import Halogen.HTML as HH
-- import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, Void, bind, const, discard, flip, pure, ($))
import UI (uiComponent)
import UI.Types (UIQuery (..))
import V (Decision)
import Vis (VVis, selectVis)
import VisColor (background)

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
    -- , viewDec: dec
    }

  render :: CState Number -> H.ParentHTML CQuery UIQuery UISlot (Aff (CEffects m))
  render state =
    HH.div
      [ HP.id_ "container"
      ]
      [ HH.slot UISlot uiComponent (state.currVis) (input Changed)
      -- [ HH.slot UISlot uiComponent (state.currVis) (const Nothing)
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
          _ <- H.liftEff (renderVis c s.currVis dec)
          pure next
        Nothing -> pure next
    Changed _ next -> do
      dec <- H.query UISlot $ H.request ViewDec
      mc <- H.liftEff $ C.getCanvasElementById "draw"
      case mc of
        Just c -> do
          s <- H.get
          _ <- H.liftEff (renderVis c s.currVis dec)
          pure next
        Nothing -> pure next

-- | The entry point for rendering a visualization.  This function queries the
-- | view decision from the UI, performs a selection on the visualization, and
-- | passes that on into the drawing module.
renderVis :: forall m.
  CanvasElement -> VVis Number -> Maybe Decision -> Eff (CEffects m) Context2D
renderVis can v' mdec = do
  w <- C.getCanvasWidth can
  h <- C.getCanvasHeight can
  c <- C.getContext2D can
  _ <- translate {translateX: 0.5, translateY: 0.5 } c
  let v = maybe v' (flip selectVis v') mdec
  _ <- parseVis c v (Box { x: 0.0, y: 0.0, w: w, h: h})
  pure c


--------------------------------------------------------------------------------
-- Style definitions

-- | The style for the actual HTML canvas we render to.
canvasStyle :: StyleM Unit
canvasStyle = do
  width (pct 69.0)
  height (pct 100.0)
  float floatLeft
  backgroundColor white

-- | The style for the outer UI container.
containerStyle :: StyleM Unit
containerStyle = do
  width (pct 100.0)
  height (pct 100.0)
  overflow hidden
  backgroundColor background
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
  fontFamily ["helvetica"] (singleton sansSerif)
