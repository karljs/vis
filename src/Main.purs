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
  , plain1
  , exp2
  , hyb1
  , hyb2
  , hyb3
  , stk
  ) where

import Color.Scheme.MaterialDesign
import Vis

import Canvas (cComponent)
import Color (Color, rgba, toRGBA)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Loops (whileJust)
import DOM.HTML.Types (htmlElementToNode)
import DOM.Node.Node (firstChild, removeChild)
import Data.Array (sort)
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Math (ln2, log, sqrt)
import Prelude (Unit, bind, map, negate, ($), (/))
import V (V(..))
import Vis.Types (Frame(..), VVis(..), above, fillsH, fillsW, nextTo, overlay, overlayFlat, stacks)
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
vs3 = [ One 3.1, One (1.0), One (1.2), One 1.7, One 1.2, One 2.9, One 2.4
      , One (2.5), One 2.1, One 2.3 ]

vs3log :: Array (V Number)
vs3log = map One $ map log [ 3.1, (1.0), (1.2), 1.7, 1.2, 2.9, 2.4 , (2.5), 2.1, 2.3 ]

vs3sqrt :: Array (V Number)
vs3sqrt = map One $ map sqrt [ 3.1, (1.0), (1.2), 1.7, 1.2, 2.9, 2.4 , (2.5), 2.1, 2.3 ]

vs4 :: Array (V Number)
vs4 = [ One 0.4, One 0.8, One 1.2, One 1.6, One 2.0]

vs5 :: Array (V Number)
vs5 = [ One 0.3, One 0.4, One 1.0, One 2.1, One 1.0]

v1 :: VVis Number
v1 = NextTo { vs: fillsH vs1 }

v2 :: VVis Number
v2 = MkPolar v1

v3 :: VVis Number
v3 = Above { vs: fillsW vs2 }

k :: VVis Number
k = NextTo { vs: fillsH vs3 }

v4 :: VVis Number
v4 = NextTo { vs: fillsH vs4 }

v5 :: VVis Number
v5 = NextTo { vs: fillsH vs5 }

tblue :: Color
tblue = let b = toRGBA blue
        in rgba b.r b.g b.b 0.5

tgreen :: Color
tgreen = let b = toRGBA green
         in rgba b.r b.g b.b 0.5

overTest :: VVis Number
overTest =
  overlayFlat
    (v4 `space` 0.25 `rightSpace` 0.02 `color1` tgreen)
    (v5 `space` 0.25 `leftSpace` 0.02)

plain1 :: VVis Number
plain1 = NextTo { vs: fillsH vs3 }

plain1log :: VVis Number
plain1log = NextTo { vs: fillsH vs3log }

plain1sqrt :: VVis Number
plain1sqrt = NextTo { vs: fillsH vs3sqrt }

deets :: Array (V Number)
deets = [ Chc "Region 1" (One 0.4) (One 0.4)
        , Chc "Region 2" (One 1.0) (One 1.0)
        , Chc "Region 3" (One 0.9) (One 0.9)
        , Chc "Region 4" (One 1.6) (One 1.6)
        , Chc "Region 5" (One 0.3) (One 0.3) ]

exp2 :: VVis Number
exp2 = (reorient $ MkPolar $ NextTo { vs: fillsH deets }) `color` defaultColors

vs3s :: Array (V Number)
vs3s = map One $ sort [ 3.1, (1.0), (1.2), 1.7, 1.2, 2.9, 2.4, (2.5), 2.1, 2.3 ]

ps2 :: Array (V Number)
ps2 = [ One 4.1, (One (-1.0)), (One 2.0), One 2.3, One (-1.2),
        One 1.3, (One 3.4), (One 4.2), One 0.8, One 2.9 ]

ps2s :: Array (V Number)
ps2s = map One $ sort [ 4.1, (-1.0), 2.0, 2.3, (-1.2), 1.3,  3.4,  4.2, 0.8, 2.9 ]


plain2 :: VVis Number
plain2 = NextTo { vs: fillsH ps2 }

plain1s :: VVis Number
plain1s = NextTo { vs: fillsH vs3s }

plain2s :: VVis Number
plain2s = NextTo { vs: fillsH ps2s }

hyb1 :: VVis Number
hyb1 = V "Sorted" (above [plain1, plain2 `color1` blue])
                  (overlayFlat (plain1s `color1` tgreen) (plain2s `color1` blue))

recip :: Number -> Number
recip n = 1.0 / n

lp2 :: Array (V Number)
lp2 = map One $ map recip [ 4.1, (-1.0), 2.0, 2.3, (-1.2), 1.3,  3.4,  4.2, 0.8, 2.9 ]

lp2s :: Array (V Number)
lp2s = map One $ sort $ map recip [ 4.1, (-1.0), 2.0, 2.3, (-1.2), 1.3,  3.4,  4.2, 0.8, 2.9 ]

vlp2 :: VVis Number
vlp2 = NextTo { vs: fillsH lp2 }

vlp2s :: VVis Number
vlp2s = NextTo { vs: fillsH lp2s }

hyb2 :: VVis Number
hyb2 = above [ nextTo [plain2, vlp2] `space` 0.2
             , nextTo [plain2s, vlp2s] `space` 0.2]

-- hyb2 = V "Sorted" (V "Log" (plain2) (vlp2)) (V "Log" (plain2s) (vlp2s))

hyb3 :: VVis Number
hyb3 = V "TxType" (overlayFlat (plain1 `color1` tgreen) (plain1log)) (overlayFlat (plain1 `color1` tgreen) (plain1sqrt))



stk :: VVis Number
stk = NextTo { vs: stacks [1.0,2.0,3.0] [2.0,3.0,4.0]}