module Vis.Types
  ( FillRec
  , Frame(..)
  , Label(..)
  , LabelPositionH(..)
  , LabelPositionV(..)
  , Orientation(..)
  , VPs(..)
  , VVis(..)
  ) where

import Color (Color)
import Data.List.NonEmpty (NonEmptyList, foldr)
import Data.Maybe (Maybe)
import Data.Show (class Show, show)
import Data.Tuple (Tuple)
import Prelude (map, (<>))
import V (Dim)

-- | The primary type of a visualization
data VVis a
  = Fill (FillRec a)
  | V Dim (VVis a) (VVis a)
  | NextTo (NonEmptyList (VVis a))
  | Above (NonEmptyList (VVis a))
  | Cartesian (VVis a)
  | Polar (VVis a)
  | Overlay (NonEmptyList (VVis a))
  | Stacked (NonEmptyList (VVis a))

type FillRec a =
  { vps :: VPs
  , frameH :: Frame a
  , frameW :: Frame a
  , label :: Maybe Label
  }

instance showVVis :: Show a => Show (VVis a) where
  show (Fill f) = "Fill " <> show f.vps <> " "
                          <> "H: " <> show f.frameH <> " "
                          <> "W: " <> show f.frameW <> " "
                          <> "label " <> show f.label
  show (V d l r) = "V " <> d <> "\n" <> show l <> "\n" <> show r <> "\nEnd: " <> d
  show (NextTo vs) =
    "NextTo\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show vs))
  show (Above vs) =
    "Above\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show vs))
  show (Cartesian v) = "Cartesian\n" <> show v
  show (Polar v) = "Polar\n" <> show v
  show (Overlay vs) =
    "Overlay\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show vs))
  show (Stacked vs) =
    "Stacked\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show vs))

data VPs = VPs
  { height :: Number
  , width :: Number
  , color :: Color
  , visible :: Boolean
  , orientation :: Orientation
  }

instance showVPs :: Show VPs where
  show (VPs vp) = "(" <> show vp.height <> "," <> show vp.width <> ")"

-- | A frame represents the context into which we map the values being charted.
-- | Currently this just tracks the minimum and maximum values in a chart.
data Frame a = Frame
  { frameMax :: a
  , frameMin :: a
  }

instance showFrame :: Show a => Show (Frame a) where
  show (Frame f) = show f.frameMin <> "--" <> show f.frameMax

data Orientation = Vertical | Horizontal

--------------------------------------------------------------------------------
-- Everything to do with labels

data Label = Label
  { text :: String
  , position :: Tuple LabelPositionV LabelPositionH
  , size :: Number
  }

instance showLabel :: Show Label where
  show (Label l) = "Label: " <> l.text

data LabelPositionV = VPosTop | VPosMiddle | VPosBottom

data LabelPositionH = HPosLeft | HPosMiddle | HPosRight
