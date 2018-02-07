module UI.Types
  ( DecisionColors(..)
  , UIInput
  , UIMessage (..)
  , UIState
  , UIQuery (..)
  , mkDecColors
  ) where

import Color (Color)
import Data.List (List, zip)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple)
import V (Decision, Dim)
import Vis (VVis)

--------------------------------------------------------------------------------
-- The types for the UI component

-- | The state for the child UI component, which tracks the view decision for
-- | a variational visualization.
type UIState a = { viewDec :: Decision
                 , vis :: VVis a
                 , dimColors :: DecisionColors
                 }

-- | The query algebra for the UI component.  It supports toggling a dimension
-- | in the view decision and querying the view decision.
data UIQuery a
  = ToggleL Dim a
  | ToggleR Dim a
  | OnOff Dim a
  | ViewDec (Tuple Decision DecisionColors -> a)

-- | The input to the UI component is just a visualization, from which we
-- | can extract a default view decision.
type UIInput a = VVis a

-- | When a change is made to the view decision, send a message with the new
-- | decision and the decision colors.
data UIMessage = Toggled Decision DecisionColors

-- | A mapping from dimensions to colors, which helps map parts of the canvas to
-- | the actual dimension names in the UI.
type DecisionColors = Map Dim Color

-- | From (ordered) lists of dimensions and colors, produce a proper mapping.
mkDecColors :: List Dim -> List Color -> DecisionColors
mkDecColors ds cs = fromFoldable (zip ds cs)