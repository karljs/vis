module Canvas.Types
  ( CEffects
  , CInput
  , CQuery (..)
  , CState
  , Space (..)
  , UISlot (..)
  ) where

import Graphics.Canvas (CANVAS)
import Prelude (class Eq, class Ord, Void)
import Vis.Types (Rectangle, VVis)

-- | A type corresponding to a part of a canvas that we can render to, which
-- | is used to partition the space as we match complicated visualizations.
data Space
  = Box Rectangle


--------------------------------------------------------------------------------
-- Types used for the canvas component

-- | An extensible record that includes the CANVAS effect, which we use to
-- | draw things to an HTML canvas.
type CEffects eff = (canvas :: CANVAS | eff)

-- | The state for the canvas component, which is currently just a variational
-- | visualization.  The view decision comes from querying the child UI.
type CState a = { currVis :: VVis a
                }

-- | The query algebra for the canvas component.  The only thing it knows how
-- | to deal with is a request to render, which is issued at initialization.
data CQuery a
  = Render a
  | Changed Void a

-- | The input to the canvas component, which is the visualization to be
-- | rendered.
type CInput a = VVis a

-- | A type for the child component of the canvas component, which is the UI
-- | which allows the view decision to be set.
data UISlot = UISlot
derive instance eqUISlot :: Eq UISlot
derive instance ordUISlot :: Ord UISlot
