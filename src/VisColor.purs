module VisColor
  ( makeDimColors
  ) where

import Color (Color)
import Color.Scale (colors, spectrumLCh)
import Data.List (List)
import Data.List.Lazy (take)
import Prelude ((+))

makeDimColors :: Int -> List Color
makeDimColors n = colors spectrumLCh (n + 1)