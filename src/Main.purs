module Main
  ( module V
  , module Vis
  , module Vis.Types
  , go
  , main
  , v1
  , v2
  , vs1
  , vs2
  ) where

import Canvas (cComponent)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Loops (whileJust)
import DOM.HTML.Types (htmlElementToNode)
import DOM.Node.Node (firstChild, removeChild)
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, negate, ($))
import V (V(..))
import Vis (Orientation(..), reorient, rotate)
import Vis.Types (Frame(..), VVis(..), above, fillsH, fillsV, nextTo)

main :: Eff (HA.HalogenEffects (canvas :: CANVAS, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI cComponent defaultVis body

-- | When run as an executable with `main` as the entry point, this is the
-- | visualization that will be rendered.  It's just for convenience.
defaultVis :: VVis Number
defaultVis = v1

-- | Kind of like `main`, except it does the extra step of deleting all child
-- | HTML nodes from the body.  This is useful when running from a REPL.
go :: VVis Number -> Eff (HA.HalogenEffects (canvas :: CANVAS, console :: CONSOLE)) Unit
go vis = HA.runHalogenAff do
  body <- HA.awaitBody
  let nb = htmlElementToNode body
  _ <- H.liftEff $ whileJust
         (firstChild (htmlElementToNode body))
         (\n -> removeChild n (htmlElementToNode body))
  runUI cComponent vis body

--------------------------------------------------------------------------------
-- Some test values for charting

vs1 :: Array (V Number)
vs1 = [ One 8.1, Chc "Dim1" (One (-3.0)) (One 6.0), One 2.3, One (-5.2),
        One 1.3, Chc "Dim2" (One 3.4) (One 5.2) ]

vs2 :: Array (V Number)
vs2 = [ One 2.3, One 6.8, One (-1.0), Chc "Dim3" (One 1.0) (One 4.0),
        Chc "Dim4" (Chc "Dim5" (One 5.0) (One (-2.0))) (One 3.2) ]

v1 :: VVis Number
v1 = MkPolar $ NextTo { orientation: OrientVertical
                      , vs: fillsV vs1
                      }

v2 :: VVis Number
v2 = Above { orientation: OrientHorizontal
           , vs: fillsH vs2
           }
