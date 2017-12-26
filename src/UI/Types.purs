module UI.Types
  ( UIInput
  , UIState
  , UIQuery (..)
  ) where

import V (Decision, Dim)
import Vis (VVis)

--------------------------------------------------------------------------------
-- The types for the UI component

-- | The state for the child UI component, which tracks the view decision for
-- | a variational visualization.
type UIState a = { viewDec :: Decision
                 , vis :: VVis a}

-- | The query algebra for the UI component.  It supports toggling a dimension
-- | in the view decision and querying the view decision.
data UIQuery a
  = Toggle Dim a
  | ViewDec (Decision -> a)

-- | The input to the UI component is just a visualization, from which we
-- | can extract a default view decision.
type UIInput a = VVis a
