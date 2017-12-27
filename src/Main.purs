module Main where

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
import Vis.Types (VVis, fills, nextTo)

main :: Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI cComponent defaultVis body

-- | When run as an executable with `main` as the entry point, this is the
-- | visualization that will be rendered.  It's just for convenience.
defaultVis :: VVis Number
defaultVis = test

vs :: List (V Number)
vs = One 8.1 : Chc "negative" (One (-8.0)) (One 8.0) : One 2.3 : One (-5.2) :
     One 0.3 : One 3.4 : Nil

test :: VVis Number
test = nextTo $ fills vs

test2 :: VVis Number
test2 = nextTo $ fills (reverse vs)

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