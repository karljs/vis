module Vis
  ( module Vis.Types

  , color
  , color1
  , flop
  , mapFill
  , removeCoord
  , reorient
  , rotate
  , vsort
  , vZipWith
  , minusHeight
  , plusHeight
  , setFrames

  , space
  , leftSpace
  , rightSpace
  , topSpace
  , bottomSpace

  , selectVis
  , selectVisM
  , visDims
  , visInitDec

  , isVisible
  , getColor
  , getHeight
  , getWidth
  , getOrientation
  , splitPosNeg
  , visMaxH
  , visMaxW
  , visMinH
  , visMinW

  , above
  , above'
  , fills
  , fillsH
  , fillsW
  , nextTo
  , overlay
  , overlayFlat
  , stack
  , stacks
  , hspace
  , vspace
  , vPie
  , vBarchart
  ) where

import Color (Color, white)
import Color.Scheme.MaterialDesign (green)
import Data.Array as A
import Data.Foldable as F
import Data.List (List(Nil), concatMap, filter, nub, partition, (:)) as L
import Data.List.NonEmpty (NonEmptyList, cons, fromFoldable, fromList, head, singleton, sortBy, toList, zipWith) as NE
import Data.Map (empty)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String (take)
import Data.Tuple (Tuple(..))
import Math (min)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, comparing, flip, map, max, show, ($), (+), (-), (<), (<<<), (<>), (==), (||))
import Util (doUnsafeListOp, intersperse, maximum, minimum, unsafeMaybe, unsafeNonEmpty, vmaximum, vminimum) as U
import V (Decision, Dim, Dir(..), V(..), lookupDim, plainVals)
import Vis.Types (Frame(..), Label(..), LabelPositionH(..), LabelPositionV(..), Orientation(..), VPs(..), VVis(..), FillRec)

--------------------------------------------------------------------------------
-- Generic traversals
mapFill :: (FillRec -> FillRec) -> VVis -> VVis
mapFill f (Fill fr) = Fill $ f fr
mapFill f (V d l r) = V d (mapFill f l) (mapFill f r)
mapFill f (NextTo vs) = NextTo $ map (mapFill f) vs
mapFill f (Above vs) = Above $ map (mapFill f) vs
mapFill f (Cartesian v) = Cartesian $ mapFill f v
mapFill f (Polar v) = Polar $ mapFill f v
mapFill f (Overlay vs) = Overlay $ map (mapFill f) vs
mapFill f (Stacked vs) = Stacked $ map (mapFill f) vs

mapVPs :: forall a b. (VPs -> VPs) -> VVis -> VVis
mapVPs f = let ff fr = fr { vps = f fr.vps }
           in mapFill ff


--------------------------------------------------------------------------------
-- Transformations

-- | Change the orientation between vertical and horizontal, or angle and radius
reorient :: forall a. VVis -> VVis
reorient =  mapFill f where
  f fr = fr { frameW = fr.frameH
            , frameH = fr.frameW
            , vps = swapWH fr.vps }

swapWH :: VPs -> VPs
swapWH (VPs vp) =
  VPs (vp { width = vp.height
          , height = vp.width
          , orientation = swapO vp.orientation })
  where swapO Vertical = Horizontal
        swapO Horizontal = Vertical

-- | Change the direction of composition
flop :: forall a. VVis -> VVis
flop (Fill f) = Fill f
flop (V d l r) = V d (flop l) (flop r)
flop (NextTo vs) = Above $ map flop vs
flop (Above vs) = NextTo $ map flop vs
flop (Cartesian v) = Cartesian (flop v)
flop (Polar v) = Polar (flop v)
flop (Overlay vs) = Overlay $ map flop vs
flop (Stacked vs) = Stacked $ map flop vs

-- | Flop, then reorient
rotate :: forall a. VVis -> VVis
rotate = reorient <<< flop

vsort :: VVis -> VVis
vsort (NextTo vs) = NextTo $ NE.sortBy (comparing guessMain) vs
vsort (Above vs) = Above $ NE.sortBy (comparing guessMain) vs
vsort (Polar v) = Polar $ vsort v
vsort (Cartesian v) = Cartesian $ vsort v
vsort (V d l r) = V d (vsort l) (vsort r)
vsort v = v

guessMain :: VVis -> Number
guessMain (Fill v) = case getOrientation v.vps of
  Vertical -> getHeight v.vps
  Horizontal -> getWidth v.vps
guessMain _ = 0.0

vZipWith :: (VPs -> VPs -> VPs) -> VVis -> VVis -> VVis
vZipWith f v1 v2 =
  let v3 = vz f v1 v2
      fh = Frame { frameMin: min 0.0 (visMinH v3)
                 , frameMax: max 0.0 (visMaxH v3) }
      fw = Frame { frameMin: min 0.0 (visMinW v3)
                 , frameMax: max 0.0 (visMaxW v3) }
  -- in fixLabels $ setFrames fh fw v3 where
  in setFrames fh fw v3 where
    vz :: (VPs -> VPs -> VPs) -> VVis -> VVis -> VVis
    vz f (NextTo vs1) (NextTo vs2) = NextTo $ zipFills f vs1 vs2
    vz f (Above vs1) (Above vs2) = Above $ zipFills f vs1 vs2
    vz f (Cartesian v1) v2 = Cartesian (vz f v1 v2)
    vz f v1 (Cartesian v2) = Cartesian (vz f v1 v2)
    vz f (Polar v1) v2 = Polar (vz f v1 v2)
    vz f v1 (Polar v2) = Polar (vz f v1 v2)
    vz f (V d1 l1 r1) (V d2 l2 r2) | d1 == d2 =
      V d1 (vz f l1 l2) (vz f r1 r2)
    vz f (V d l r) v = V d (vz f l v) (vz f r v)
    vz f v (V d l r) = V d (vz f l v) (vz f r v)
    vz _ v _ = v

zipFills ::
  (VPs -> VPs -> VPs) ->
  NE.NonEmptyList (VVis) ->
  NE.NonEmptyList (VVis) ->
  NE.NonEmptyList (VVis)
zipFills f v1 v2 = NE.zipWith (getVPsZip f) v1 v2

getVPsZip ::
  (VPs -> VPs -> VPs) ->
  VVis -> VVis -> VVis
getVPsZip f (Fill v1) (Fill v2) = Fill (v1 { vps = f v1.vps v2.vps })
getVPsZip _ v _ = v

fixLabels :: VVis -> VVis
fixLabels = mapFill f where
  f fr = let l = case getOrientation fr.vps of
                   Vertical -> defaultLabel (getHeight fr.vps)
                   Horizontal -> defaultLabel (getWidth fr.vps)
         in (fr { label = Just l })

minusHeight :: VPs -> VPs -> VPs
minusHeight (VPs v1) (VPs v2) =
  VPs (v1 { height = v1.height - v2.height })

plusHeight :: VPs -> VPs -> VPs
plusHeight (VPs v1) (VPs v2) =
  VPs (v1 { height = v1.height + v2.height })

-- | Iterate over a visualization and remove all the constructors that change
-- | the coordinate system.
removeCoord :: forall a. VVis -> VVis
removeCoord (Fill f) = Fill f
removeCoord (V d l r) = V d (removeCoord l) (removeCoord r)
removeCoord (NextTo vs) = NextTo $ map removeCoord vs
removeCoord (Above vs) = Above $ map removeCoord vs
removeCoord (Cartesian v) = removeCoord v
removeCoord (Polar v) = removeCoord v
removeCoord (Overlay vs) = Overlay $ map removeCoord vs
removeCoord (Stacked vs) = Stacked $ map removeCoord vs

space :: VVis -> Number -> VVis
space (NextTo vs) n = NextTo $ U.doUnsafeListOp (U.intersperse (hspace n)) vs
space (Above vs) n = Above $ U.doUnsafeListOp (U.intersperse (vspace n)) vs
space (V d l r) n = V d (space l n) (space r n)
space (Cartesian v) n = Cartesian (space v n)
space (Polar v) n = Polar (space v n)
space (Overlay vs) n = Overlay $ U.doUnsafeListOp (U.intersperse (vspace n)) vs
space (Stacked v) _ = Stacked v
space (Fill v) _ = Fill v

leftSpace :: VVis -> Number -> VVis
leftSpace v n = NextTo (NE.cons (hspace n) (NE.singleton v))

rightSpace :: VVis -> Number -> VVis
rightSpace v n = NextTo (NE.cons v (NE.singleton (hspace n)))

topSpace :: VVis -> Number -> VVis
topSpace v n = Above (NE.cons (vspace n) (NE.singleton v))

bottomSpace :: VVis -> Number -> VVis
bottomSpace v n = Above (NE.cons v (NE.singleton (vspace n)))


--------------------------------------------------------------------------------
-- Aesthetics and style functions

color :: forall a. VVis -> NE.NonEmptyList Color -> VVis
color (Fill f) cs = let (VPs vps) = f.vps
                    in Fill (f { vps = (VPs (vps { color = NE.head cs } )) })
color (V d l r) cs = V d (color l cs) (color r cs)
color (NextTo vs) cs = NextTo $ NE.zipWith color1 vs cs
color (Above vs) cs = Above $ NE.zipWith color1 vs cs
color (Cartesian v) cs = Cartesian (color v cs)
color (Polar v) cs = Polar (color v cs)
color (Overlay vs) cs = Overlay $ NE.zipWith color1 vs cs
color (Stacked vs) cs = Stacked $ NE.zipWith color1 vs cs

color1 :: forall a. VVis -> Color -> VVis
color1 v c = mapVPs (f c) v where
  f c (VPs vps) = VPs (vps { color = c })

--------------------------------------------------------------------------------
-- Things related to variability

-- | Select with a maybe `Decision`.
selectVisM :: Maybe Decision -> VVis -> VVis
selectVisM md v = maybe v (flip selectVis v) md


-- | Perform selection on a variational visualization.
selectVis :: forall a. Decision -> VVis -> VVis
selectVis _ (Fill f) = Fill f
selectVis dec (V d l r) = case lookupDim d dec of
  Just L -> selectVis dec l
  Just R -> selectVis dec r
  Nothing -> V d (selectVis dec l) (selectVis dec r)
selectVis dec (NextTo vs) = NextTo $ map (selectVis dec) vs
selectVis dec (Above vs) = Above $ map (selectVis dec) vs
selectVis dec (Cartesian v) = Cartesian (selectVis dec v)
selectVis dec (Polar v) = Polar (selectVis dec v)
selectVis dec (Overlay vs) = Overlay $ map (selectVis dec) vs
selectVis dec (Stacked vs) = Stacked $ map (selectVis dec) vs

-- | Generate an initial (view) decision for a particular visualization.
-- | Generally this will be all left selections.
visInitDec :: forall a. VVis -> Decision
visInitDec v = empty
  -- leftDec (visDims v)

-- | Extract all the dimensions from a visualization
visDims :: forall a. VVis -> L.List Dim
visDims = L.nub <<< visDimsHelp where
  visDimsHelp (Fill _) = L.Nil
  visDimsHelp (V d l r) = d L.: (visDimsHelp l <> visDimsHelp r)
  visDimsHelp (NextTo vs) = L.concatMap visDimsHelp (NE.toList vs)
  visDimsHelp (Above vs) = L.concatMap visDimsHelp (NE.toList vs)
  visDimsHelp (Cartesian v) = visDimsHelp v
  visDimsHelp (Polar v) = visDimsHelp v
  visDimsHelp (Overlay vs) = L.concatMap visDimsHelp (NE.toList vs)
  visDimsHelp (Stacked vs) = L.concatMap visDimsHelp (NE.toList vs)

--------------------------------------------------------------------------------
-- Queries

isVisible :: forall e. { vps :: VPs | e } -> Boolean
isVisible r = let (VPs vps) = r.vps
              in vps.visible

getHeight :: VPs -> Number
getHeight (VPs vps) = vps.height

getWidth :: VPs -> Number
getWidth (VPs vps) = vps.width

getColor :: VPs -> Color
getColor (VPs vps) = vps.color

getOrientation :: VPs -> Orientation
getOrientation (VPs vps) = vps.orientation

splitPosNeg ::
  L.List (VVis) ->
  Tuple (L.List (VVis)) (L.List (VVis))
splitPosNeg vs =
  let p = L.partition anyNegative vs
  in Tuple (L.filter isFill p.no) (p.yes)

anyNegative :: VVis -> Boolean
anyNegative (Fill f) =
  let (VPs vps) = f.vps
  in vps.height < 0.0 || vps.width < 0.0
anyNegative _ = false

isFill :: forall a. VVis -> Boolean
isFill (Fill _) = true
isFill _ = false

visOrientation :: forall a. VVis -> Orientation
visOrientation (Fill f) = getOrientation f.vps
visOrientation (V _ l _) = visOrientation l
visOrientation (NextTo vs) = visOrientation (NE.head vs)
visOrientation (Above vs) = visOrientation (NE.head vs)
visOrientation (Cartesian v) = visOrientation v
visOrientation (Polar v) = visOrientation v
visOrientation (Overlay vs) = visOrientation (NE.head vs)
visOrientation (Stacked vs) = visOrientation (NE.head vs)


visMaxH :: VVis -> Number
visMaxH (Fill f) = getHeight f.vps
visMaxH (V d l r) = max (visMaxH l) (visMaxH r)
visMaxH (NextTo vs) = U.maximum (map visMaxH vs)
visMaxH (Above vs) = U.maximum (map visMaxH vs)
visMaxH (Cartesian v) = visMaxH v
visMaxH (Polar v) = visMaxH v
visMaxH (Overlay vs) = U.maximum (map visMaxH vs)
visMaxH (Stacked vs) = case visOrientation (NE.head vs) of
  Vertical -> F.foldr (+) 0.0 (map visMaxH vs)
  Horizontal -> U.maximum (map visMaxH vs)

visMinH :: VVis -> Number
visMinH (Fill f) = getHeight f.vps
visMinH (V d l r) = min (visMinH l) (visMinH r)
visMinH (NextTo vs) = U.minimum (map visMinH vs)
visMinH (Above vs) = U.minimum (map visMinH vs)
visMinH (Cartesian v) = visMinH v
visMinH (Polar v) = visMinH v
visMinH (Overlay vs) = U.minimum (map visMinH vs)
visMinH (Stacked vs) = case visOrientation (NE.head vs) of
  Vertical -> F.foldr (+) 0.0 (map visMinH vs)
  Horizontal -> U.minimum (map visMinH vs)

visMaxW :: VVis -> Number
visMaxW (Fill f) = getWidth f.vps
visMaxW (V d l r) = max (visMaxW l) (visMaxW r)
visMaxW (NextTo vs) = U.maximum (map visMaxW vs)
visMaxW (Above vs) = U.maximum (map visMaxW vs)
visMaxW (Cartesian v) = visMaxW v
visMaxW (Polar v) = visMaxW v
visMaxW (Overlay vs) = U.maximum (map visMaxW vs)
visMaxW (Stacked vs) = case visOrientation (NE.head vs) of
  Horizontal -> F.foldr (+) 0.0 (map visMaxW vs)
  Vertical -> U.maximum (map visMaxW vs)

visMinW :: VVis -> Number
visMinW (Fill f) = getWidth f.vps
visMinW (V d l r) = min (visMinW l) (visMinW r)
visMinW (NextTo vs) = U.minimum (map visMinW vs)
visMinW (Above vs) = U.minimum (map visMinW vs)
visMinW (Cartesian v) = visMinW v
visMinW (Polar v) = visMinW v
visMinW (Overlay vs) = U.minimum (map visMinW vs)
visMinW (Stacked vs) = case visOrientation (NE.head vs) of
  Horizontal -> F.foldr (+) 0.0 (map visMinW vs)
  Vertical -> U.minimum (map visMinW vs)

--------------------------------------------------------------------------------
-- Helper functions for (mostly unsafely) constructing nonempty lists of things

-- | Create fill objects for an array of variational numbers where the data is
-- | bound to the height
fillsH :: Array (V Number) -> NE.NonEmptyList VVis
fillsH hsarr =
  let hs = A.toUnfoldable hsarr
      fw = Frame { frameMin: 0.0, frameMax: 1.0 }
  in fills (map (setW 1.0) hs) (genFrame hs) fw Vertical

-- | Create fill objects for an array of variational numbers where the data is
-- | bound to the width
fillsW :: Array (V Number) -> NE.NonEmptyList (VVis)
fillsW wsarr =
  let ws = A.toUnfoldable wsarr
      fh = Frame { frameMin: 0.0, frameMax: 1.0 }
  in fills (map (setH 1.0) ws) fh (genFrame ws) Horizontal

fillsWA :: Frame -> Frame -> V (Array Number) -> VVis
fillsWA fh fw (Chc d l r) = V d (fillsWA fh fw l) (fillsWA fh fw r)
fillsWA fh fw (One x) =
  let farr = map (mkFill fh fw) x
  in NextTo $ unsafePartial (fromJust (NE.fromFoldable farr)) where
    mkFill fh fw v =
      let theVPs = VPs { height: 1.0
                       , width: v
                       , color: green
                       , visible: true
                       , orientation: Horizontal
                       }
      in Fill { vps: theVPs
              , frameH: fh
              , frameW: fw
              , label: Nothing
              }

setW :: Number -> V Number -> V (Tuple Number Number)
setW w (Chc d l r) = Chc d (setW w l) (setW w r)
setW w (One h) = One (Tuple w h)

setH :: Number -> V Number -> V (Tuple Number Number)
setH h (Chc d l r) = Chc d (setH h l) (setH h r)
setH h (One w) = One (Tuple w h)

-- | An _unsafe_ helper function (when the parameter list is empty) that takes a
-- | list of variational numbers and produces a frame based on the minimum and
-- | maximum values.
genFrame :: L.List (V Number) -> Frame
genFrame vs =
  let vs'  = unsafePartial $ fromJust (NE.fromList vs)
      fmin = min (U.vminimum vs') 0.0
      fmax = max (U.vmaximum vs') 0.0
  in Frame { frameMax: fmax, frameMin: fmin }

fills ::
  L.List (V (Tuple Number Number)) ->
  Frame ->
  Frame ->
  Orientation ->
  NE.NonEmptyList (VVis)
fills vs fh fw o = U.unsafeNonEmpty $ map (genFill fh fw o) vs

-- | For a variational number, produce a `Fill` visualization.
genFill ::
  Frame ->
  Frame ->
  Orientation ->
  V (Tuple Number Number) ->
  VVis
genFill fh fw o (One (Tuple w h)) =
  let vp = VPs { height: h
               , width: w
               , color: green
               , visible: true
               , orientation: o
               }
      l = case o of
            Vertical -> defaultLabel h
            Horizontal -> defaultLabel w
  in Fill { vps: vp
          , frameH: fh
          , frameW: fw
          -- , label: Nothing
          , label: Just l
          }
genFill fh fw o (Chc d l r) = V d (genFill fh fw o l) (genFill fh fw o r)

hspace :: Number -> VVis
hspace v = spaceFill v 1.0 Horizontal

vspace :: Number -> VVis
vspace v = spaceFill 1.0 v Vertical

spaceFill :: Number -> Number -> Orientation -> VVis
spaceFill w h o =
  let vp = VPs { height: h
               , width: w
               , color: white
               , visible: false
               , orientation: o }
  in Fill { vps: vp
          , frameH: Frame { frameMin: 0.0, frameMax: 1.0 }
          , frameW: Frame { frameMin: 0.0, frameMax: 1.0 }
          , label: Nothing
          }

defaultLabel :: forall a. Show a => a -> Label
defaultLabel v = Label { text: take 4 (show v)
                       , position: Tuple VPosTop HPosMiddle
                       , size: 48.0 }

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `NextTo`.
nextTo' :: forall a. L.List (VVis) -> VVis
nextTo' vs = NextTo $ unsafePartial $ fromJust $ NE.fromList vs

nextTo :: forall a. Array (VVis) -> VVis
nextTo vs = nextTo' $ A.toUnfoldable vs

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `Above`.
above' :: forall a. L.List (VVis) -> VVis
above' vs = Above $ unsafePartial $ fromJust $ NE.fromList vs

-- | An _unsafe_ helper function (when the parameter array is empty) for
-- | composing with `Above`.
above :: forall a. Array (VVis) -> VVis
above vs = above' $ A.toUnfoldable vs

-- | Overlay the first visualization over the second
overlay :: forall a. VVis -> VVis -> VVis
overlay v1 v2 = Overlay $ NE.cons v1 (NE.singleton v2)

overlayFlat :: forall a. VVis -> VVis -> VVis
overlayFlat v1 v2 =
  let maxH = max (visMaxH v1) (visMaxH v2)
      maxW = max (visMaxW v1) (visMaxW v2)
      minH = min (visMinH v1) (visMinH v2)
      minW = min (visMinW v1) (visMinW v2)
      v1f = setFrames (Frame { frameMin: min 0.0 minH
                             , frameMax: max 0.0 maxH })
                      (Frame { frameMin: min 0.0 minW
                             , frameMax: max 0.0 maxW })
                      v1
      v2f = setFrames (Frame { frameMin: min 0.0 minH
                             , frameMax: max 0.0 maxH })
                      (Frame { frameMin: min 0.0 minW
                             , frameMax: max 0.0 maxW })
                      v2
  in Overlay $ NE.cons v1f (NE.singleton v2f)

-- | Set or overwrite the frames for an existing visualization.  Parameters
-- | have the height frame first, followed by the width frame.
setFrames :: forall a. Frame -> Frame -> VVis -> VVis
setFrames fh fw (Fill f) = Fill (f { frameH = fh, frameW = fw })
setFrames fh fw (V d l r) = V d (setFrames fh fw l) (setFrames fh fw r)
setFrames fh fw (NextTo vs) = NextTo $ map (setFrames fh fw) vs
setFrames fh fw (Above vs) = Above $ map (setFrames fh fw) vs
setFrames fh fw (Cartesian v) = Cartesian (setFrames fh fw v)
setFrames fh fw (Polar v) = Polar (setFrames fh fw v)
setFrames fh fw (Overlay vs) = Overlay $ map (setFrames fh fw) vs
setFrames fh fw (Stacked vs) = Stacked $ map (setFrames fh fw) vs

doStack :: VVis -> VVis -> VVis
doStack x (Stacked vs) = Stacked $ NE.cons x vs
doStack (V d1 l1 r1) (V d2 l2 r2) | d1 == d2 = V d1 (stack l1 r2) (stack r1 r2)
doStack (V d l r) v = V d (stack l v) (stack r v)
doStack v (V d l r) = V d (stack l v) (stack r v)
doStack (Polar x) y = Polar $ stack x y
doStack (Cartesian x) y = Cartesian $ stack x y
doStack x (Polar y) = Polar $ stack x y
doStack x (Cartesian y) = Cartesian $ stack x y
doStack x y = Stacked $ NE.cons x (NE.singleton y)

stack :: VVis -> VVis -> VVis
stack v1 v2 =
  let v3 = stackHelp v1 v2
      fh = Frame { frameMin: min 0.0 (visMinH v3)
                 , frameMax: max 0.0 (visMaxH v3) }
      fw = Frame { frameMin: min 0.0 (visMinW v3)
                 , frameMax: max 0.0 (visMaxW v3) }
  in setFrames fh fw v3 where
    stackHelp (NextTo vs1) (NextTo vs2) = NextTo $ NE.zipWith doStack vs1 vs2
    stackHelp (Above vs1) (Above vs2) = Above $ NE.zipWith doStack vs1 vs2
    stackHelp v1 v2 = doStack v1 v2

stacks :: Array Number -> Array Number -> NE.NonEmptyList (VVis)
stacks xs ys =
  let maxH = unsafePartial $ fromJust $ F.maximum $ A.zipWith (+) xs ys
      minH = unsafePartial $ fromJust $ F.minimum $ A.zipWith (+) xs ys
      fh = Frame { frameMin: min 0.0 minH, frameMax: max 0.0 maxH }
      fw = Frame { frameMin: 0.0, frameMax: 1.0 }
  in map (setFrames fh fw)
         (NE.zipWith stack2 (fillsH (map One xs)) (fillsH (map One ys)))

stack2 :: forall a. VVis -> VVis -> VVis
stack2 x y = Stacked (NE.cons x (NE.singleton y))

vBarchart :: Array (V Number) -> VVis
vBarchart = NextTo <<< fillsH

vPie :: Array (V (Array Number)) -> VVis
vPie xs =
  let mna = map (map (\a -> U.unsafeMaybe (F.minimum a))) (map plainVals xs)
      mn  = U.unsafeMaybe (F.minimum (map U.minimum mna))
      mxa = map (map (\a -> U.unsafeMaybe (F.maximum a))) (map plainVals xs)
      mx  = U.unsafeMaybe (F.maximum (map U.maximum mna))
      fh = Frame { frameMin: 0.0, frameMax: 1.0 }
      fw = Frame { frameMin: min 0.0 mn, frameMax: max 0.0 mx}
      fs = map (fillsWA fh fw) xs
  in Polar $ NextTo $ U.unsafeMaybe (NE.fromFoldable fs)

