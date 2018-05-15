module V
  ( module V.Types
  , decDims
  , emptyDec
  , leftDec
  , lookupDim
  , notInDec
  , plainVals
  , showDec
  , showDecM
  , singleDec
  , toggleDim
  , vDims
  ) where

import Data.List (List(..), (:), nub, sort)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Map (member)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Prelude (flip, map, not, ($), (<<<), (<>))
import V.Types (Decision, Dim, Dir(..), V(..))

--------------------------------------------------------------------------------
-- Select, extract, or modify parts of the V types
--------------------------------------------------------------------------------

-- | Selection reduces or eliminates variability in choice variational values.
select :: forall a. Decision -> V a -> V a
select dec (One x) = One x
select dec (Chc d l r) = case lookupDim d dec of
  Just L -> select dec l
  Just R -> select dec r
  _      -> Chc d (select dec l) (select dec r)

lookupDim :: Dim -> Decision -> Maybe Dir
lookupDim = M.lookup


-- | Extract a list of `Dim` values for which the `Decision` contains a
-- | mapping.  These will be unique.
decDims :: Decision -> List Dim
decDims dec = M.keys dec

-- | Extract a list of unique `Dim` values which are contained in a particular
-- | variational value.
vDims :: forall a. V a -> List Dim
vDims v = sort <<< nub $ dims v where
  dims (Chc d l r) = d : dims l <> dims r
  dims (One x) = Nil

-- | Switch between binary selectors, which is useful when toggling them in the
-- | view decision.
flop :: Dir -> Maybe Dir
flop L = Just R
flop R = Just L

-- | Toggle one particular dimension in a decision
toggleDim :: Dim -> Decision -> Decision
toggleDim dim dec = M.update flop dim dec

plainVals :: forall a. V a -> NonEmptyList a
plainVals (One x) = singleton x
plainVals (Chc d l r) = plainVals l <> plainVals r

notInDec :: Dim -> Decision -> Boolean
notInDec = not <<< member

--------------------------------------------------------------------------------
--- Constructing or generating particular kinds of V types.
--------------------------------------------------------------------------------

-- | An empty decision contains no selectors
emptyDec :: Decision
emptyDec = M.empty

-- | For a particular variational value, produce a decision that maps each of
-- | its dimensions to the left alternative.
leftDec :: forall a. List Dim -> Decision
leftDec ds = M.fromFoldable $ map (flip Tuple L) ds

-- | Produce a text representation of a decision.
showDec :: Decision -> String
showDec = M.showTree

-- | Produce a text representation of a decision that may or may not exist.
showDecM :: Maybe Decision -> String
showDecM = maybe "DEC ERROR" showDec

singleDec :: Tuple Dim Dir -> Decision
singleDec x = M.fromFoldable (singleton x)