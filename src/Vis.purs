module Vis
  ( module Vis.Types
  , selectVis
  , visInitDec
  ) where

import Data.List (List(..), concatMap, (:))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Prelude (map, (<>))
import V (Decision, Dim, Dir(..), leftDec, lookupDim)
import Vis.Types (Rectangle, VVis(..))

visInitDec :: forall a. VVis a -> Decision
visInitDec v = leftDec (visDims v)

visDims :: forall a. VVis a -> List Dim
visDims (Fill _ _) = Nil
visDims (V d l r) = d : visDims l <> visDims r
visDims (NextTo vs) = concatMap visDims (toList vs)

selectVis :: forall a. Decision -> VVis a -> VVis a
selectVis _   (Fill v m) = Fill v m
selectVis dec (V d l r) = case lookupDim d dec of
  Just L -> selectVis dec l
  Just R -> selectVis dec r
  Nothing -> V d (selectVis dec l) (selectVis dec r)
selectVis dec (NextTo vs) = NextTo (map (selectVis dec) vs)
