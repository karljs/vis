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
  , vs3
  , vs3s
  , plain1
  , plain1log
  , plain1sqrt
  , hyb1
  , hyb2
  , hyb3
  , stk
  , vlp2
  , vlp2s
  , vpos1
  , vpos2
  , vpos1r
  , vpos2r
  , piedet
  , threea
  , threeb
  , threec
  , foura
  , fourb
  , fivea
  , fiveb
  , fivec
  , karl
  ) where

import Color.Scheme.MaterialDesign
import Vis

import Canvas (cComponent)
import Color (Color, rgba, toRGBA)
import Color.Scheme.X11 (lightblue, lightseagreen)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Loops (whileJust)
import DOM.HTML.Types (htmlElementToNode)
import DOM.Node.Node (firstChild, removeChild)
import Data.Array (sort)
import Data.List.NonEmpty (cons, head, singleton)
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Math (log, sqrt)
import Prelude (Unit, bind, map, negate, ($), (/))
import V (V(..))
import Vis.Types (Frame(..), VVis(..))
import VisColor (defaultColors)

main :: Eff (HA.HalogenEffects (canvas :: CANVAS, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI cComponent defaultVis body

-- | When run as an executable with `main` as the entry point, this is the
-- | visualization that will be rendered.  It's just for convenience.
defaultVis :: VVis
defaultVis = v1

-- | Kind of like `main`, except it does the extra step of deleting all child
-- | HTML nodes from the body.  This is useful when running from a REPL.
go ::
  VVis -> Eff (HA.HalogenEffects (canvas :: CANVAS, console :: CONSOLE)) Unit
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

v1 :: VVis
v1 = NextTo $ fillsH vs1

v2 :: VVis
v2 = Polar v1

v3 :: VVis
v3 = Above $ fillsW vs2

k :: VVis
k = NextTo $ fillsH vs3

v4 :: VVis
v4 = NextTo $ fillsH vs4

v5 :: VVis
v5 = NextTo $ fillsH vs5

tblue :: Color
tblue = let b = toRGBA blue
        in rgba b.r b.g b.b 0.5

tgreen :: Color
tgreen = let b = toRGBA green
         in rgba b.r b.g b.b 0.5

overTest :: VVis
overTest =
  overlayFlat
    (v4 `space` 0.25 `rightSpace` 0.02 `color1` tgreen)
    (v5 `space` 0.25 `leftSpace` 0.02)

plain1 :: VVis
plain1 = NextTo $ fillsH vs3

plain1log :: VVis
plain1log = NextTo $ fillsH vs3log

plain1sqrt :: VVis
plain1sqrt = NextTo $ fillsH vs3sqrt

vs3s :: Array (V Number)
vs3s = map One $ sort [ 3.1, (1.0), (1.2), 1.7, 1.2, 2.9, 2.4, (2.5), 2.1, 2.3 ]

ps2 :: Array (V Number)
ps2 = [ One 4.1, (One (-1.0)), (One 2.0), One 2.3, One (-1.2),
        One 1.3, (One 3.4), (One 4.2), One 0.8, One 2.9 ]

ps2s :: Array (V Number)
ps2s = map One $ sort [ 4.1, (-1.0), 2.0, 2.3, (-1.2), 1.3,  3.4,  4.2, 0.8, 2.9 ]


plain2 :: VVis
plain2 = NextTo $ fillsH ps2

plain1s :: VVis
plain1s = NextTo $ fillsH vs3s

plain2s :: VVis
plain2s = NextTo $ fillsH ps2s

hyb1 :: VVis
hyb1 = V "Sorted" (above [plain1, plain2 `color1` blue])
                  (overlayFlat (plain1s `color1` tgreen) (plain2s `color1` blue))

recip :: Number -> Number
recip n = 1.0 / n

lp2 :: Array (V Number)
lp2 = map One $ map recip [ 4.1, (-1.0), 2.0, 2.3, (-1.2), 1.3,  3.4,  4.2, 0.8, 2.9 ]

lp2s :: Array (V Number)
lp2s = map One $ sort $ map recip [ 4.1, (-1.0), 2.0, 2.3, (-1.2), 1.3,  3.4,  4.2, 0.8, 2.9 ]

vlp2 :: VVis
vlp2 = NextTo $ fillsH lp2

vlp2s :: VVis
vlp2s = NextTo $ fillsH lp2s

hyb2 :: VVis
hyb2 = above [ nextTo [plain2, vlp2] `space` 0.2
             , nextTo [plain2s, vlp2s] `space` 0.2]

-- hyb2 = V "Sorted" (V "Log" (plain2) (vlp2)) (V "Log" (plain2s) (vlp2s))

hyb3 :: VVis
hyb3 = V "TxType" (overlayFlat (plain1 `color1` tgreen) (plain1log)) (overlayFlat (plain1 `color1` tgreen) (plain1sqrt))



stk :: VVis
stk = NextTo $ stacks [(-1.0),(-2.0),3.0] [2.0,(-3.0),4.0]

vpos1 :: VVis
vpos1 =
  let v = Polar $ NextTo $ fillsW (map One [8.5,6.3,7.3,6.3,3.9,9.7,7.5,9.6,7.9,9.6])
  in v `color` defaultColors

vpos2 :: VVis
vpos2 =
  let v = Polar $ NextTo $ fillsW (map One [6.6,1.3,5.5,1.5,8.1,5.0,7.1,6.4,7.3,8.8])
  in v `color` defaultColors

vpos1r :: VVis
vpos1r =
  let v = NextTo $ fillsH (map One [8.5,6.3,7.3,6.3,3.9,9.7,7.5,9.6,7.9,9.6])
  in v `color` defaultColors

vpos2r :: VVis
vpos2r =
  let v = NextTo $ fillsH (map One [6.6,1.3,5.5,1.5,8.1,5.0,7.1,6.4,7.3,8.8])
  in v `color` defaultColors

piedet :: VVis
piedet =
  let fh  = Frame {frameMin: 0.0, frameMax: 1.0}
      fw  = Frame {frameMin: 0.0, frameMax: 3.0}
      v1  = head (fillsW [One 3.0]) `color1` green
      v11 = head (fillsW [One 1.9]) `color1` lightGreen
      v12 = head (fillsW [One 0.7]) `color1` lightseagreen
      v13 = head (fillsW [One 0.4]) `color1` green
      v2  = head (fillsW [One 2.0]) `color1` blue
      v21 = head (fillsW [One 1.0]) `color1` lightblue
      v22 = head (fillsW [One 0.4]) `color1` blueGrey
      v23 = head (fillsW [One 0.6]) `color1` blue
      v3  = head (fillsW [One 0.8]) `color1` amber
      v31 = head (fillsW [One 0.2]) `color1` orange
      v32 = head (fillsW [One 0.3]) `color1` deepOrange
      v33 = head (fillsW [One 0.3]) `color1` amber
      right1 = NextTo $ cons v11 (cons v12 (singleton v13))
      right2 = NextTo $ cons v21 (cons v22 (singleton v23))
      right3 = NextTo $ cons v31 (cons v32 (singleton v33))
      x1 = V "Region 1" v1 right1
      x2 = V "Region 2" v2 right2
      x3 = V "Region 3" v3 right3
  in setFrames fh fw $
       Polar $
         NextTo $ cons x1 (cons x2 (singleton x3))

karl :: VVis
karl = vPie [ Chc "Region 1" (One [3.0]) (One [1.9, 0.7, 0.4])
            , Chc "Region 2" (One [2.0]) (One [1.0, 0.4, 0.6])
            , Chc "Region 3" (One [0.8]) (One [0.2, 0.3, 0.3])]

threea :: VVis
threea = v1

threeb :: VVis
threeb = above [v5 `color` defaultColors, Polar $ reorient v5 `color` defaultColors] `space` 0.1

threec :: VVis
threec = Polar $ above [plain1 `color1` orange, vlp2 `color1` blue]

foura :: VVis
foura = overlayFlat (v4 `color1` tblue) v5

fourb :: VVis
fourb = overTest

fivea :: VVis
fivea = nextTo [above [vpos1r, vpos2r] `space` 0.2, vZipWith minusHeight vpos1r vpos2r] `space` 0.2

fiveb :: VVis
fiveb = V "TxType" (overlayFlat (plain1log `color1` tblue) (plain1 `color1` green)) (overlayFlat (plain1sqrt `color1` tblue) (plain1 `color1` green))

fivech1 :: VVis
fivech1 =
  let v = Polar $ NextTo $ fillsW (map One [1.4, 0.7, 1.9, 1.2])
  in v `color` defaultColors

fivech2 :: VVis
fivech2 =
  let v = Polar $ NextTo $ fillsW (map One [1.0, 2.1, 0.9])
  in v `color` defaultColors


fivec :: VVis
fivec = V "Sorted" (Polar $ above [fivech1, fivech2]) (Polar $ above [vsort fivech1, vsort fivech2])
