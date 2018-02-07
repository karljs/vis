module UI (module UI.Types, uiComponent) where

import CSS (StyleM, background, backgroundColor, bold, border, display, float, floatRight, fontWeight, height, inlineBlock, margin, marginBottom, marginLeft, marginRight, marginTop, pct, px, solid, white, width)
import Color (Color, black)
import Data.Array ((:))
import Data.Foldable (length)
import Data.List (toUnfoldable)
import Data.Map (delete, insert, lookup, member)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (type (~>), Unit, bind, const, discard, map, pure, (<>), (==))
import UI.Types (UIInput, UIMessage(..), UIQuery(..), UIState, DecisionColors, mkDecColors)
import V (Dim, Dir(..))
import Vis (VVis, visDims, visInitDec)
import VisColor (makeDimColors)

-- | The main UI child component which generates an interface for working with
-- | the view decision.
uiComponent :: forall a m. H.Component HH.HTML UIQuery (UIInput a) UIMessage m
uiComponent =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: VVis a -> UIState a
  initialState v =
    { viewDec: visInitDec v
    , vis: v
    , dimColors: assignColors v
    }

  render :: UIState a -> H.ComponentHTML UIQuery
  render state =
    HH.div
        [ HP.id_ "dims"
        , style dimsStyle
        ]
        [ HH.fieldset
          [ style fieldsetStyle
          ]
          ( HH.legend_ [ HH.h2_ [ HH.text "Dimensions" ] ] :
            map (dimBox state) (toUnfoldable (visDims state.vis)) )
        -- , HH.text (showDec state.viewDec)
        ]

  eval :: UIQuery ~> H.ComponentDSL (UIState a) UIQuery UIMessage m
  eval = case _ of
    ToggleL dim next -> do
      state <- H.get
      let togState = setViewDec L dim state
      H.put togState
      H.raise (Toggled togState.viewDec togState.dimColors)
      pure next
    ToggleR dim next -> do
      state <- H.get
      let togState = setViewDec R dim state
      H.put togState
      H.raise (Toggled togState.viewDec togState.dimColors)
      pure next
    OnOff dim next -> do
      state <- H.get
      let togState = toggleViewDec dim state
      H.put togState
      H.raise (Toggled togState.viewDec togState.dimColors)
      pure next
    ViewDec reply -> do
      state <- H.get
      pure (reply (Tuple state.viewDec state.dimColors))

-- | A UI component helper that generates a checkbox and, if it's checked, radio
-- | buttons for a particular dimension.
dimBox :: forall a. UIState a -> Dim -> H.ComponentHTML UIQuery
dimBox st d =
  let col = case lookup d st.dimColors of
            Just c -> c
            _ -> black
  in HH.label [ style (labelStyle col) ]
              ([ HH.text d
               , HH.input [ style inputStyle
                          , HP.type_ HP.InputCheckbox
                          , HP.title d
                          , HE.onChecked (HE.input_ (OnOff d)) ]
               , HH.text "  " ] <>
               case lookup d st.viewDec of
                 Just dir -> radios d dir
                 Nothing  -> [ HH.br_ ])

-- Generate the nested radio buttons that appear when a dimension is toggled on
radios :: forall a. Dim -> Dir -> Array (HH.HTML a (UIQuery Unit))
radios d dir =
  [ HH.input ([ style inputStyle
              , HP.type_ HP.InputRadio
              , HP.name d
              , HE.onChecked (HE.input_ (ToggleL d))
              , HP.id_ (d <> "left")
              ] <> if dir == L
                  then [ HP.checked true ]
                  else [ ])
  , HH.label [ HP.for (d <> "left") ] [ HH.text "Left" ]
  , HH.input ([ style inputStyle
              , HP.type_ HP.InputRadio
              , HP.name d
              , HE.onChecked (HE.input_ (ToggleR d))
              , HP.id_ (d <> "right")
              ] <> if dir == R
                  then [ HP.checked true ]
                  else [ ])
  , HH.label [ HP.for (d <> "right") ] [ HH.text "Right" ]
  , HH.br_ ]


--------------------------------------------------------------------------------
-- Helper functions for implementing the UI component

-- | Set a particular dimension direction in the state of the UI component.
setViewDec :: forall a. Dir -> Dim -> UIState a -> UIState a
setViewDec dir dim st = st { viewDec = insert dim dir st.viewDec }

-- | Toggle, i.e. add or remove a dimension from the state of the UI.
toggleViewDec :: forall a. Dim -> UIState a -> UIState a
toggleViewDec dim st = if member dim st.viewDec
                       then st { viewDec = delete dim st.viewDec }
                       else st { viewDec = insert dim L st.viewDec }

--------------------------------------------------------------------------------
-- Style definitions

-- | The style for the div that contains the dimension toggling UI.
dimsStyle :: StyleM Unit
dimsStyle = do
  width (pct 30.0)
  height (pct 100.0)
  display inlineBlock
  backgroundColor white
  float floatRight
  -- border solid (px 1.0) background
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)

-- | The style for the fieldset containing the dimension checkboxes.
fieldsetStyle :: StyleM Unit
fieldsetStyle = do
  marginTop (px 0.0)
  marginBottom (px 10.0)
  border solid (px 1.0) black
  fontWeight bold

-- | The style for the UI labels, which helps map colors to the visualization
labelStyle :: Color -> StyleM Unit
labelStyle c = do
  background c

-- | The style for the checkboxes themselves.
inputStyle :: StyleM Unit
inputStyle = do
  marginLeft  (px 10.0)
  marginRight (px 5.0)

-- | Create a color mapping for the dimensions in a particular visualization
assignColors :: forall a. VVis a -> DecisionColors
assignColors vis =
  let ds = visDims vis
      cs = makeDimColors (length ds)
  in mkDecColors ds cs