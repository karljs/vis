module VisColor
  ( defaultColors
  , makeDimColors
  ) where

import Color (Color, rgb)
import Color.Scale (colors, spectrumLCh)
import Data.List (List(..), concat, (:))
import Data.List.NonEmpty (fromList)
import Data.List.Types (NonEmptyList)
import Data.Maybe (fromJust)
import Data.Unfoldable (replicate)
import Partial.Unsafe (unsafePartial)
import Prelude ((+))

makeDimColors :: Int -> List Color
makeDimColors n = colors spectrumLCh (n + 1)

defaultColors :: NonEmptyList Color
defaultColors =
  unsafePartial (fromJust (fromList
    (concat (replicate 3
      ( rgb 166 206 227
      : rgb 31  120 180
      : rgb 178 223 138
      : rgb 51  160 44
      : rgb 251 154 153
      : rgb 227 26  28
      : rgb 253 191 111
      : rgb 255 127 0
      : rgb 202 178 214
      : rgb 106 61  154
      : rgb 255 255 153
      : rgb 177 89  40
      : Nil )))))
