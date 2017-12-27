module Head
  ( headComponent
  ) where

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Halogen as H
import Halogen.HTML as HH
import Prelude

-- | This component just inserts a stylesheet for the html body, because all the
-- | stuff we are inserting otherwise is below the body.
headComponent :: forall m. H.Component HH.HTML (Const Void) Unit Void m
headComponent =
  H.component
    { initialState: const unit
    , render: const $ HH.style_ [ HH.text "body { margin: 0; }"]
    , eval: absurd <<< unwrap
    , receiver: const Nothing
    }