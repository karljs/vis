module Vis.Types
  ( Frame(..)
  , Rectangle
  , VVis(..)
  , fill
  , fills
  , nextTo
  ) where

import Data.List.Types (NonEmptyList)
import Prelude (class Ord, flip, map, (<), (>))
import Util (maximum, minimum)
import V (Dim)

-- | The primary type of a visualization
data VVis a
  = Fill a (Frame a)
  | V Dim (VVis a) (VVis a)
  | NextTo (NonEmptyList (VVis a))

data Frame a = Frame
  { frameMax :: a
  , frameMin :: a
  }

type Rectangle = { x :: Number
                 , y :: Number
                 , w :: Number
                 , h :: Number
                 }

-- | A smart constructor for visualization marks.
fill :: forall a. a -> Frame a -> VVis a
fill = Fill

fills :: NonEmptyList Number -> NonEmptyList (VVis Number)
fills vs =
  let mx = maximum vs
      mn = minimum vs
      f = Frame { frameMax: if mx > 0.0 then mx else 0.0
                , frameMin: if mn < 0.0 then mn else 0.0
                }
  in map (flip Fill f) vs

nextTo :: forall a. NonEmptyList (VVis a) -> VVis a
nextTo = NextTo
