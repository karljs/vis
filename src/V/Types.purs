module V.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Prelude (class Show)

-- | Dimensions give names to choices and synchronize choices that share
-- | dimensions.
type Dim = String

-- | The variational type constructor, implemented using the choice calculus.
data V a = Chc Dim (V a) (V a) | One a

derive instance genericV :: Generic (V a) _
instance showV :: Show a => Show (V a) where
  show = genericShow

-- | Since we only use binary choices, selectors will just use left or right
-- | directions.
data Dir = L | R

instance showDir :: Show Dir where
  show L = "L"
  show R = "R"

-- | Decisions encode sets of selectors
type Decision = M.Map Dim Dir
