module Vis.Types
  ( Frame(..)
  , Rectangle
  , VVis(..)
  , fill
  , fills
  , nextTo
  ) where

import Data.List.NonEmpty (fromList)
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (class Ord, flip, map, ($), (<), (>))
import Util (maximum, minimum, vmaximum, vminimum)
import V (Dim, V(..))

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

fills :: List (V Number) -> NonEmptyList (VVis Number)
fills vs =
  let vs' = unsafePartial $ fromJust (fromList vs)
      f = Frame { frameMax: vmaximum vs', frameMin: vminimum vs' }
  in map (mkFill f) vs'

mkFill :: Frame Number -> V Number -> VVis Number
mkFill f (One v) = Fill v f
mkFill f (Chc d l r) = V d (mkFill f l) (mkFill f r)

  -- let mx = maximum vs
  --     mn = minimum vs
  --     f = Frame { frameMax: if mx > 0.0 then mx else 0.0
  --               , frameMin: if mn < 0.0 then mn else 0.0
  --               }
  -- in map (flip Fill f) vs

nextTo :: forall a. NonEmptyList (VVis a) -> VVis a
nextTo = NextTo
