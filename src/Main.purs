module Main where

import Canvas (cComponent)
import Control.Monad.Eff (Eff)
import Control.Monad.Loops (whileJust)
import DOM.HTML.Types (htmlElementToNode)
import DOM.Node.Node (firstChild, removeChild)
import Data.List.NonEmpty (cons, singleton)
import Graphics.Canvas (CANVAS)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, ($), bind, negate)
import Vis.Types (Frame(..), VVis (..), fills, nextTo)

main :: Eff (HA.HalogenEffects (canvas :: CANVAS)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI cComponent defaultVis body

-- | When run as an executable with `main` as the entry point, this is the
-- | visualization that will be rendered.  It's just for convenience.
defaultVis :: VVis Number
defaultVis =
  let f = Frame { frameMin: -4.0, frameMax: 12.0 }
  in NextTo $ (cons (V "A" (Fill 2.0 f) (V "B" (Fill 10.5 f) (Fill 8.0 f)))
                (cons (Fill 12.0 f)
                  (cons (Fill (-1.0) f)
                    (singleton (Fill (-4.0) f)))))

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
