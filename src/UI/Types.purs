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
import Data.Map (Map, empty, fromFoldable)
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
  = Toggle Dim a
  | ViewDec (Tuple Decision DecisionColors -> a)

-- | The input to the UI component is just a visualization, from which we
-- | can extract a default view decision.
type UIInput a = VVis a

data UIMessage = Toggled Decision DecisionColors

type DecisionColors = Map Dim Color

mkDecColors :: List Dim -> List Color -> DecisionColors
mkDecColors ds cs = fromFoldable (zip ds cs)