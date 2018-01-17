module Vis
  ( module Vis.Types
  , selectVis
  , selectVisM
  , visDims
  , visInitDec
  ) where

import Data.List (List(..), concatMap, (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..), maybe)
import Prelude (flip, map, (<>))
import V (Decision, Dim, Dir(..), leftDec, lookupDim)
import Vis.Types (VVis(..))

-- | Select with a maybe `Decision`.
selectVisM :: Maybe Decision -> VVis Number -> VVis Number
selectVisM md v = maybe v (flip selectVis v) md

-- | Perform selection on a variational visualization.
selectVis :: forall a. Decision -> VVis a -> VVis a
selectVis _   (Fill v m) = Fill v m
selectVis dec (V d l r) = case lookupDim d dec of
  Just L -> selectVis dec l
  Just R -> selectVis dec r
  Nothing -> V d (selectVis dec l) (selectVis dec r)
selectVis dec (NextTo vs) = NextTo (map (selectVis dec) vs)
selectVis dec (Above vs) = Above (map (selectVis dec) vs)

-- | Generate an initial (view) decision for a particular visualization.
-- | Generally this will be all left selections.
visInitDec :: forall a. VVis a -> Decision
visInitDec v = leftDec (visDims v)

-- | Extract all the dimensions from a visualization
visDims :: forall a. VVis a -> List Dim
visDims (Fill _ _) = Nil
visDims (V d l r) = d : visDims l <> visDims r
visDims (NextTo vs) = concatMap visDims (toList vs)
visDims (Above vs) = concatMap visDims (toList vs)
