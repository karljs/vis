module Canvas.Types
  ( class Renderable
  , CEffects
  , CInput
  , CQuery(..)
  , CState
  , Drawing(..)
  , Primitive(..)
  , Rectangle(..)
  , Space(..)
  , UISlot(..)
  , render
  ) where

import DOM (DOM)
import Data.List (List)
import Data.Show (class Show, show)
import Graphics.Canvas (CANVAS)
import Prelude (class Eq, class Ord, (<>))
import UI.Types (UIMessage)
import Vis.Types (VVis)

class Renderable r where
  render :: forall a. r a -> Drawing

data Drawing = Drawing (List Primitive)

data Primitive
  = Box Rectangle

-- | A type corresponding to a part of a canvas that we can render to, which
-- | is used to partition the space as we match complicated visualizations.
data Space
  = Cartesian Rectangle

-- | A rectangle is represented as a top left corner plus a width and height.
data Rectangle = Rectangle
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }

instance showRectangle :: Show Rectangle where
  show (Rectangle r) = "Rectangle (" <> show r.x <> ", " <> show r.y <> ") "
                                     <> show r.w <> " " <> show r.h


--------------------------------------------------------------------------------
-- Types used for the canvas component

-- | An extensible record that includes the CANVAS effect, which we use to
-- | draw things to an HTML canvas.
type CEffects eff = (canvas :: CANVAS, dom :: DOM | eff)

-- | The state for the canvas component, which is currently just a variational
-- | visualization.  The view decision comes from querying the child UI.
type CState a = { currVis :: VVis a
                }

-- | The query algebra for the canvas component.  The only thing it knows how
-- | to deal with is a request to render, which is issued at initialization.
data CQuery a
  = Render a
  | Changed UIMessage a

-- | The input to the canvas component, which is the visualization to be
-- | rendered.
type CInput a = VVis a

-- | A type for the child component of the canvas component, which is the UI
-- | which allows the view decision to be set.
data UISlot = UISlot
derive instance eqUISlot :: Eq UISlot
derive instance ordUISlot :: Ord UISlot
