module Vis.Types
  ( FillRec
  , Frame(..)
  , Label(..)
  , LabelPositionH(..)
  , LabelPositionV(..)
  , Orientation(..)
  , VPs(..)
  , VVis(..)
  , ppv
  ) where

import Color (Color)
import Data.List.NonEmpty (NonEmptyList, foldr)
import Data.Maybe (Maybe)
import Data.Show (class Show, show)
import Data.Tuple (Tuple)
import Prelude (map, (<>))
import V (Dim)

-- | The primary type of a visualization
data VVis
  = Fill FillRec
  | V Dim VVis VVis
  | NextTo (NonEmptyList VVis)
  | Above (NonEmptyList VVis)
  | Cartesian VVis
  | Polar VVis
  | Overlay (NonEmptyList VVis)
  | Stacked (NonEmptyList VVis)

type FillRec =
  { vps :: VPs
  , frameH :: Frame
  , frameW :: Frame
  , label :: Maybe Label
  }

instance showVVis :: Show VVis where
  show (Fill f) = "Fill " <> show f.vps <> " "
                          <> "H: " <> show f.frameH <> " "
                          <> "W: " <> show f.frameW <> " "
                          <> "label " <> show f.label
  show (V d l r) = "V " <> d <> " left\n" <> show l <> "right\n" <> show r <> "\nEnd: " <> d
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

ppv :: VVis -> String
ppv (V d l r) = d <> "<" <> ppv l <> ", " <> ppv r <> ">"
ppv (Fill _) = "v"
ppv (Above vs) = "abv[]"
ppv (NextTo _) = "nxt[]"
ppv (Overlay _) = "ovr[]"
ppv (Stacked _) = "stk[]"
ppv (Cartesian _) = "crt[]"
ppv (Polar _) = "pol[]"

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
data Frame = Frame
  { frameMax :: Number
  , frameMin :: Number
  }

instance showFrame :: Show Frame where
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
