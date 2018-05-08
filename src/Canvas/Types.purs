module Canvas.Types
  ( CEffects
  , CInput
  , CQuery(..)
  , CState
  , Rectangle(..)
  , Space(..)
  , UISlot(..)
  , Wedge(..)
  , unitRect
  ) where

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Show (class Show, show)
import Graphics.Canvas (CANVAS)
import Prelude (class Eq, class Ord, (<>))
import UI.Types (UIMessage)
import Vis.Types (VVis)

-- | A type corresponding to a part of a canvas that we can render to, which
-- | is used to partition the space as we match complicated visualizations.
data Space
  = SpaceCartesian Rectangle
  | SpacePolar Wedge

instance showSpace :: Show Space where
  show (SpaceCartesian r) = "SpaceCartesian: " <> show r
  show (SpacePolar w) = "SpacePolar: " <> show w

-- | A rectangle is represented as a top left corner plus a width and height.
-- |
-- | - The upper left corner coordinates `x` and `y`
-- | - The width and height `w` and `h`
data Rectangle = Rectangle
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }

unitRect :: Rectangle
unitRect = Rectangle { x: 0.0, y: 0.0, w: 1.0, h: 1.0 }

instance showRectangle :: Show Rectangle where
  show (Rectangle r) = "Rectangle (" <> show r.x <> ", " <> show r.y <> ") "
                                     <> show r.w <> " " <> show r.h

-- | A wedge represents a chunk of SpacePolar space.  It could be anything from a
-- | circle to a piece of a doughnut.
-- |
-- | - The coordinates of the center of the corresponding circle `x` and `y`
-- | - The inner and outer radius of the wedge `inRad` and `outRad`
-- | - The starting and ending angle `startAngle` and `endAngle`
data Wedge = Wedge
  { x :: Number
  , y :: Number
  , inRad :: Number
  , outRad :: Number
  , startAngle :: Number
  , endAngle :: Number
  }

instance showWedge :: Show Wedge where
  show (Wedge w) = "Wedge (" <> show w.x <> ", " <> show w.y <> ")"
                             <> " in: " <> show w.inRad
                             <> " out: " <> show w.outRad
                             <> " a1: " <> show w.startAngle
                             <> " a2: " <> show w.endAngle


--------------------------------------------------------------------------------
-- Types used for the Halogen canvas component

-- | An extensible record that includes the CANVAS effect, which we use to
-- | draw things to an HTML canvas.
type CEffects eff = (canvas :: CANVAS, dom :: DOM, console :: CONSOLE | eff)

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
