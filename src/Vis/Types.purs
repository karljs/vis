module Vis.Types
  ( Rectangle
  , VVis (..)
  , fill
  , fills
  , nextTo
  ) where

import Data.List.Types (NonEmptyList)
import Prelude (class Ord, flip, map)
import Util (maximum)
import V (Dim)

-- | The primary type of a visualization
data VVis a
  = Fill a a
  | V Dim (VVis a) (VVis a)
  | NextTo (NonEmptyList (VVis a))

type Rectangle = { x :: Number
                 , y :: Number
                 , w :: Number
                 , h :: Number
                 }

-- | A smart constructor for visualization marks.
fill :: forall a. a -> a -> VVis a
fill = Fill

fills :: forall a. (Ord a) => NonEmptyList a -> NonEmptyList (VVis a)
fills vs =
  let m = maximum vs
  in map (flip Fill m) vs

nextTo :: forall a. NonEmptyList (VVis a) -> VVis a
nextTo = NextTo
