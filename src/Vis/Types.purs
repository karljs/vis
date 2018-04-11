module Vis.Types
  ( Frame(..)
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
  = Fill { vps :: VPs
         , frameH :: Frame a
         , frameW :: Frame a
         , label :: Maybe Label
         }
  | V Dim (VVis a) (VVis a)
  | NextTo { vs :: (NonEmptyList (VVis a)) }
  | Above { vs :: (NonEmptyList (VVis a)) }
  | MkCartesian (VVis a)
  | MkPolar (VVis a)
  | Overlay { vs :: (NonEmptyList (VVis a)) }
  | Stacked { vs :: (NonEmptyList (VVis a)) }

instance showVVis :: Show a => Show (VVis a) where
  show (Fill f) = "Fill " <> show f.vps <> " "
                          <> "H: " <> show f.frameH <> " "
                          <> "W: " <> show f.frameW <> " "
                          <> "label " <> show f.label
  show (V d l r) = "V " <> d <> " " <> show l <> " " <> show r
  show (NextTo v) =
    "NextTo\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show v.vs))
  show (Above v) =
    "Above\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show v.vs))
  show (MkCartesian v) = "Cartesian\n" <> show v
  show (MkPolar v) = "Polar\n" <> show v
  show (Overlay o) =
    "Overlay\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show o.vs))
  show (Stacked s) =
    "Stacked\n" <> (foldr (\x xs -> (x <> "\n" <> xs)) "\n" (map show s.vs))

data VPs = VPs
  { height :: Maybe Number
  , width :: Maybe Number
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
