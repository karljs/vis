module Main
  ( module Color.Scheme.MaterialDesign
  , module Prelude
  , module V
  , module Vis
  , module Vis.Types
  , module VisColor
  , go
  , k
  , main
  , overTest
  , tblue
  , tgreen
  , v1
  , v2
  , v3
  , v4
  , v5
  , vs1
  , vs2
  ) where

import Canvas (cComponent)
import Color (Color, rgba, toRGBA)
import Color.Scheme.MaterialDesign
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Loops (whileJust)
import DOM.HTML.Types (htmlElementToNode)
import DOM.Node.Node (firstChild, removeChild)
import Data.Array (reverse)
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, negate, ($))
import V (V(..))
import Vis
import Vis.Types (Frame(..), VVis(..), above, fillsH, fillsV, nextTo, overlay)
import VisColor (defaultColors)

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
go ::
  VVis Number ->
  Eff (HA.HalogenEffects (canvas :: CANVAS, console :: CONSOLE)) Unit
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

vs3 :: Array (V Number)
vs3 = [ One 3.1, One (-1.0), One (-1.2), One 1.7, One 1.2, One 2.9, One 2.4
      , One (-2.5), One 2.1, One 2.3 ]

vs4 :: Array (V Number)
vs4 = [ One 0.4, One 0.8, One 1.2, One 1.6, One 2.0]

v1 :: VVis Number
v1 = NextTo { orientation: OrientVertical
            , vs: fillsV vs1
            }

v2 :: VVis Number
v2 = MkPolar v1

v3 :: VVis Number
v3 = Above { orientation: OrientHorizontal
           , vs: fillsH vs2
           }

k :: VVis Number
k = NextTo { orientation: OrientVertical, vs: fillsV vs3 }

v4 :: VVis Number
v4 = NextTo { orientation: OrientVertical
            , vs: fillsV vs4
            }

v5 :: VVis Number
v5 = NextTo { orientation: OrientVertical
            , vs: fillsV (reverse vs4)
            }

tblue :: Color
tblue = let b = toRGBA blue
        in rgba b.r b.g b.b 0.5

tgreen :: Color
tgreen = let b = toRGBA green
         in rgba b.r b.g b.b 0.5

overTest :: VVis Number
overTest = overlay (v5 `color1` tblue `topSpace` 0.1 `rightSpace` 0.1)
                   (v4 `bottomSpace` 0.1 `leftSpace` 0.1)