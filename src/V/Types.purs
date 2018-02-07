module V.Types where

import Data.Map as M
import Prelude (class Eq, class Show)

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

instance eqDir :: Eq Dir where
  eq L L = true
  eq R R = true
  eq _ _ = false

-- | Decisions encode sets of selectors
type Decision = M.Map Dim Dir
