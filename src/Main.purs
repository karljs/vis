module Main
  ( module V
  , module Vis
  , module Vis.Types
  , go
  , main
  , v1
  , v2
  , vs
  ) where

import Canvas (cComponent)
import Control.Monad.Eff (Eff)
import Control.Monad.Loops (whileJust)
import DOM.HTML.Types (htmlElementToNode)
import DOM.Node.Node (firstChild, removeChild)
import Data.List (List(..), reverse, (:))
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, negate, ($))
import V (V(..))
import Vis
import Vis.Types (Frame(..), VVis(..), above, above', fills, nextTo, nextTo')

main :: Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI cComponent defaultVis body

-- | When run as an executable with `main` as the entry point, this is the
-- | visualization that will be rendered.  It's just for convenience.
defaultVis :: VVis Number
defaultVis = v1

-- | Kind of like `main`, except it does the extra step of deleting all child
-- | HTML nodes from the body.  This is useful when running from a REPL.
go :: VVis Number -> Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
go vis = HA.runHalogenAff do
  body <- HA.awaitBody
  let nb = htmlElementToNode body
  _ <- H.liftEff $ whileJust
         (firstChild (htmlElementToNode body))
         (\n -> removeChild n (htmlElementToNode body))
  runUI cComponent vis body

--------------------------------------------------------------------------------
-- Some test values for charting

vs :: List (V Number)
vs = One 8.1 : Chc "Dim1" (One (-3.0)) (One 8.0) : One 2.3 : One (-5.2) :
     One 1.3 : Chc "Dim2" (One 3.4) (One 4.2) : Nil

v1 :: VVis Number
v1 = NextTo $ fills vs

v2 :: VVis Number
v2 = NextTo $ fills (reverse vs)
