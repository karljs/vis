module V.Types where

import Prelude (class Show)
import Data.Map as M

-- | Dimensions give names to choices and synchronize choices that share
-- | dimensions.
type Dim = String

-- | The variational type constructor, implemented using the choice calculus.
data V a = Chc Dim (V a) (V a) | One a

-- | Since we only use binary choices, selectors will just use left or right
-- | directions.
data Dir = L | R

instance showDir :: Show Dir where
  show L = "L"
  show R = "R"

-- | Decisions encode sets of selectors
type Decision = M.Map Dim Dir