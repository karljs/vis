module Vis
  ( module Vis.Types

  , color
  , color1
  , flop
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
import Vis.Types (Frame(..), Label(..), LabelPositionH(..), LabelPositionV(..), Orientation(..), VPs(..), VVis(..))

--------------------------------------------------------------------------------
-- Transformations

-- | Change the orientation between vertical and horizontal, or angle and radius
reorient :: forall a. VVis a -> VVis a
reorient (Fill f) = Fill (f { vps = swapWH f.vps
                            , frameW = f.frameH
                            , frameH = f.frameW
                            })
reorient (V d l r) = V d (reorient l) (reorient r)
reorient (NextTo vs) = NextTo $ map reorient vs
reorient (Above vs) = Above $ map reorient vs
reorient (MkCartesian v) = MkCartesian (reorient v)
reorient (MkPolar v) = MkPolar (reorient v)
reorient (Overlay vs) = Overlay $ map reorient vs
reorient (Stacked vs) = Stacked $ map reorient vs

swapWH :: VPs -> VPs
swapWH (VPs vp) =
  VPs (vp { width = vp.height
          , height = vp.width
          , orientation = swapO vp.orientation })
  where swapO Vertical = Horizontal
        swapO Horizontal = Vertical

-- | Change the direction of composition
flop :: forall a. VVis a -> VVis a
flop (Fill f) = Fill f
flop (V d l r) = V d (flop l) (flop r)
flop (NextTo vs) = Above $ map flop vs
flop (Above vs) = NextTo $ map flop vs
flop (MkCartesian v) = MkCartesian (flop v)
flop (MkPolar v) = MkPolar (flop v)
flop (Overlay vs) = Overlay $ map flop vs
flop (Stacked vs) = Stacked $ map flop vs

-- | Flop, then reorient
rotate :: forall a. VVis a -> VVis a
rotate = reorient <<< flop

vsort :: VVis Number -> VVis Number
vsort (NextTo vs) = NextTo $ NE.sortBy (comparing guessMain) vs
vsort (Above vs) = Above $ NE.sortBy (comparing guessMain) vs
vsort (MkPolar v) = MkPolar $ vsort v
vsort (MkCartesian v) = MkCartesian $ vsort v
vsort (V d l r) = V d (vsort l) (vsort r)
vsort v = v

guessMain :: VVis Number -> Number
guessMain (Fill v) = case getOrientation v.vps of
  Vertical -> getHeight v.vps
  Horizontal -> getWidth v.vps
guessMain _ = 0.0

vZipWith :: (VPs -> VPs -> VPs) -> VVis Number -> VVis Number -> VVis Number
vZipWith f v1 v2 =
  let v3 = vz f v1 v2
      fh = Frame { frameMin: min 0.0 (visMinH v3)
                 , frameMax: max 0.0 (visMaxH v3) }
      fw = Frame { frameMin: min 0.0 (visMinW v3)
                 , frameMax: max 0.0 (visMaxW v3) }
  -- in fixLabels $ setFrames fh fw v3 where
  in setFrames fh fw v3 where
    vz :: (VPs -> VPs -> VPs) -> VVis Number -> VVis Number -> VVis Number
    vz f (NextTo vs1) (NextTo vs2) = NextTo $ zipFills f vs1 vs2
    vz f (Above vs1) (Above vs2) = Above $ zipFills f vs1 vs2
    vz f (MkCartesian v1) v2 = MkCartesian (vz f v1 v2)
    vz f v1 (MkCartesian v2) = MkCartesian (vz f v1 v2)
    vz f (MkPolar v1) v2 = MkPolar (vz f v1 v2)
    vz f v1 (MkPolar v2) = MkPolar (vz f v1 v2)
    vz f (V d1 l1 r1) (V d2 l2 r2) | d1 == d2 =
      V d1 (vz f l1 l2) (vz f r1 r2)
    vz f (V d l r) v = V d (vz f l v) (vz f r v)
    vz f v (V d l r) = V d (vz f l v) (vz f r v)
    vz _ v _ = v

zipFills ::
  (VPs -> VPs -> VPs) ->
  NE.NonEmptyList (VVis Number) ->
  NE.NonEmptyList (VVis Number) ->
  NE.NonEmptyList (VVis Number)
zipFills f v1 v2 = NE.zipWith (getVPsZip f) v1 v2

getVPsZip ::
  (VPs -> VPs -> VPs) ->
  VVis Number -> VVis Number -> VVis Number
getVPsZip f (Fill v1) (Fill v2) = Fill (v1 { vps = f v1.vps v2.vps })
getVPsZip _ v _ = v

fixLabels :: VVis Number -> VVis Number
fixLabels (Fill v) =
  let l = case getOrientation v.vps of
            Vertical -> defaultLabel (getHeight v.vps)
            Horizontal -> defaultLabel (getWidth v.vps)
  in Fill (v { label = Just l })
fixLabels (NextTo vs) = NextTo $ map fixLabels vs
fixLabels (Above vs) = Above $ map fixLabels vs
fixLabels (Overlay vs) = Overlay $ map fixLabels vs
fixLabels (Stacked vs) = Stacked $ map fixLabels vs
fixLabels (V d l r) = V d (fixLabels l) (fixLabels r)
fixLabels (MkCartesian v) = MkCartesian (fixLabels v)
fixLabels (MkPolar v) = MkPolar (fixLabels v)

minusHeight :: VPs -> VPs -> VPs
minusHeight (VPs v1) (VPs v2) =
  VPs (v1 { height = v1.height - v2.height })

plusHeight :: VPs -> VPs -> VPs
plusHeight (VPs v1) (VPs v2) =
  VPs (v1 { height = v1.height + v2.height })

-- | Iterate over a visualization and remove all the constructors that change
-- | the coordinate system.
removeCoord :: forall a. VVis a -> VVis a
removeCoord (Fill f) = Fill f
removeCoord (V d l r) = V d (removeCoord l) (removeCoord r)
removeCoord (NextTo vs) = NextTo $ map removeCoord vs
removeCoord (Above vs) = Above $ map removeCoord vs
removeCoord (MkCartesian v) = removeCoord v
removeCoord (MkPolar v) = removeCoord v
removeCoord (Overlay vs) = Overlay $ map removeCoord vs
removeCoord (Stacked vs) = Stacked $ map removeCoord vs

space :: VVis Number -> Number -> VVis Number
space (NextTo vs) n = NextTo $ U.doUnsafeListOp (U.intersperse (hspace n)) vs
space (Above vs) n = Above $ U.doUnsafeListOp (U.intersperse (vspace n)) vs
space (V d l r) n = V d (space l n) (space r n)
space (MkCartesian v) n = MkCartesian (space v n)
space (MkPolar v) n = MkPolar (space v n)
space (Overlay vs) n = Overlay $ U.doUnsafeListOp (U.intersperse (vspace n)) vs
space (Stacked v) _ = Stacked v
space (Fill v) _ = Fill v

leftSpace :: VVis Number -> Number -> VVis Number
leftSpace v n = NextTo (NE.cons (hspace n) (NE.singleton v))

rightSpace :: VVis Number -> Number -> VVis Number
rightSpace v n = NextTo (NE.cons v (NE.singleton (hspace n)))

topSpace :: VVis Number -> Number -> VVis Number
topSpace v n = Above (NE.cons (vspace n) (NE.singleton v))

bottomSpace :: VVis Number -> Number -> VVis Number
bottomSpace v n = Above (NE.cons v (NE.singleton (vspace n)))


--------------------------------------------------------------------------------
-- Aesthetics and style functions

color :: forall a. VVis a -> NE.NonEmptyList Color -> VVis a
color (Fill f) cs = let (VPs vps) = f.vps
                    in Fill (f { vps = (VPs (vps { color = NE.head cs } )) })
color (V d l r) cs = V d (color l cs) (color r cs)
color (NextTo vs) cs = NextTo $ NE.zipWith color1 vs cs
color (Above vs) cs = Above $ NE.zipWith color1 vs cs
color (MkCartesian v) cs = MkCartesian (color v cs)
color (MkPolar v) cs = MkPolar (color v cs)
color (Overlay vs) cs = Overlay $ NE.zipWith color1 vs cs
color (Stacked vs) cs = Stacked $ NE.zipWith color1 vs cs

color1 :: forall a. VVis a -> Color -> VVis a
color1 (Fill f) c = let (VPs vps) = f.vps
                    in Fill (f { vps = (VPs (vps { color = c } )) })
color1 (V d l r) c = V d (color1 l c) (color1 r c)
color1 (NextTo vs) c = NextTo $ map (flip color1 c) vs
color1 (Above vs) c = Above $ map (flip color1 c) vs
color1 (MkCartesian v) c = MkCartesian (color1 v c)
color1 (MkPolar v) c = MkPolar (color1 v c)
color1 (Overlay vs) c = Overlay $ map (flip color1 c) vs
color1 (Stacked vs) c = Stacked $ map (flip color1 c) vs

--------------------------------------------------------------------------------
-- Things related to variability

-- | Select with a maybe `Decision`.
selectVisM :: Maybe Decision -> VVis Number -> VVis Number
selectVisM md v = maybe v (flip selectVis v) md


-- | Perform selection on a variational visualization.
selectVis :: forall a. Decision -> VVis a -> VVis a
selectVis _ (Fill f) = Fill f
selectVis dec (V d l r) = case lookupDim d dec of
  Just L -> selectVis dec l
  Just R -> selectVis dec r
  Nothing -> V d (selectVis dec l) (selectVis dec r)
selectVis dec (NextTo vs) = NextTo $ map (selectVis dec) vs
selectVis dec (Above vs) = Above $ map (selectVis dec) vs
selectVis dec (MkCartesian v) = MkCartesian (selectVis dec v)
selectVis dec (MkPolar v) = MkPolar (selectVis dec v)
selectVis dec (Overlay vs) = Overlay $ map (selectVis dec) vs
selectVis dec (Stacked vs) = Stacked $ map (selectVis dec) vs

-- | Generate an initial (view) decision for a particular visualization.
-- | Generally this will be all left selections.
visInitDec :: forall a. VVis a -> Decision
visInitDec v = empty
  -- leftDec (visDims v)

-- | Extract all the dimensions from a visualization
visDims :: forall a. VVis a -> L.List Dim
visDims = L.nub <<< visDimsHelp where
  visDimsHelp (Fill _) = L.Nil
  visDimsHelp (V d l r) = d L.: (visDimsHelp l <> visDimsHelp r)
  visDimsHelp (NextTo vs) = L.concatMap visDimsHelp (NE.toList vs)
  visDimsHelp (Above vs) = L.concatMap visDimsHelp (NE.toList vs)
  visDimsHelp (MkCartesian v) = visDimsHelp v
  visDimsHelp (MkPolar v) = visDimsHelp v
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
  L.List (VVis Number) ->
  Tuple (L.List (VVis Number)) (L.List (VVis Number))
splitPosNeg vs =
  let p = L.partition anyNegative vs
  in Tuple (L.filter isFill p.no) (p.yes)

anyNegative :: VVis Number -> Boolean
anyNegative (Fill f) =
  let (VPs vps) = f.vps
  in vps.height < 0.0 || vps.width < 0.0
anyNegative _ = false

isFill :: forall a. VVis a -> Boolean
isFill (Fill _) = true
isFill _ = false

visOrientation :: forall a. VVis a -> Orientation
visOrientation (Fill f) = getOrientation f.vps
visOrientation (V _ l _) = visOrientation l
visOrientation (NextTo vs) = visOrientation (NE.head vs)
visOrientation (Above vs) = visOrientation (NE.head vs)
visOrientation (MkCartesian v) = visOrientation v
visOrientation (MkPolar v) = visOrientation v
visOrientation (Overlay vs) = visOrientation (NE.head vs)
visOrientation (Stacked vs) = visOrientation (NE.head vs)


visMaxH :: VVis Number -> Number
visMaxH (Fill f) = getHeight f.vps
visMaxH (V d l r) = max (visMaxH l) (visMaxH r)
visMaxH (NextTo vs) = U.maximum (map visMaxH vs)
visMaxH (Above vs) = U.maximum (map visMaxH vs)
visMaxH (MkCartesian v) = visMaxH v
visMaxH (MkPolar v) = visMaxH v
visMaxH (Overlay vs) = U.maximum (map visMaxH vs)
visMaxH (Stacked vs) = case visOrientation (NE.head vs) of
  Vertical -> F.foldr (+) 0.0 (map visMaxH vs)
  Horizontal -> U.maximum (map visMaxH vs)

visMinH :: VVis Number -> Number
visMinH (Fill f) = getHeight f.vps
visMinH (V d l r) = min (visMinH l) (visMinH r)
visMinH (NextTo vs) = U.minimum (map visMinH vs)
visMinH (Above vs) = U.minimum (map visMinH vs)
visMinH (MkCartesian v) = visMinH v
visMinH (MkPolar v) = visMinH v
visMinH (Overlay vs) = U.minimum (map visMinH vs)
visMinH (Stacked vs) = case visOrientation (NE.head vs) of
  Vertical -> F.foldr (+) 0.0 (map visMinH vs)
  Horizontal -> U.minimum (map visMinH vs)

visMaxW :: VVis Number -> Number
visMaxW (Fill f) = getWidth f.vps
visMaxW (V d l r) = max (visMaxW l) (visMaxW r)
visMaxW (NextTo vs) = U.maximum (map visMaxW vs)
visMaxW (Above vs) = U.maximum (map visMaxW vs)
visMaxW (MkCartesian v) = visMaxW v
visMaxW (MkPolar v) = visMaxW v
visMaxW (Overlay vs) = U.maximum (map visMaxW vs)
visMaxW (Stacked vs) = case visOrientation (NE.head vs) of
  Horizontal -> F.foldr (+) 0.0 (map visMaxW vs)
  Vertical -> U.maximum (map visMaxW vs)

visMinW :: VVis Number -> Number
visMinW (Fill f) = getWidth f.vps
visMinW (V d l r) = min (visMinW l) (visMinW r)
visMinW (NextTo vs) = U.minimum (map visMinW vs)
visMinW (Above vs) = U.minimum (map visMinW vs)
visMinW (MkCartesian v) = visMinW v
visMinW (MkPolar v) = visMinW v
visMinW (Overlay vs) = U.minimum (map visMinW vs)
visMinW (Stacked vs) = case visOrientation (NE.head vs) of
  Horizontal -> F.foldr (+) 0.0 (map visMinW vs)
  Vertical -> U.minimum (map visMinW vs)

--------------------------------------------------------------------------------
-- Helper functions for (mostly unsafely) constructing nonempty lists of things

-- barchart :: NE.NonEmptyList (VVis Number)
-- barchart = NextTo <<< fillsH

-- | Create fill objects for an array of variational numbers where the data is
-- | bound to the height
fillsH :: Array (V Number) -> NE.NonEmptyList (VVis Number)
fillsH hsarr =
  let hs = A.toUnfoldable hsarr
      fw = Frame { frameMin: 0.0, frameMax: 1.0 }
  in fills (map (setW 1.0) hs) (genFrame hs) fw Vertical

-- | Create fill objects for an array of variational numbers where the data is
-- | bound to the width
fillsW :: Array (V Number) -> NE.NonEmptyList (VVis Number)
fillsW wsarr =
  let ws = A.toUnfoldable wsarr
      fh = Frame { frameMin: 0.0, frameMax: 1.0 }
  in fills (map (setH 1.0) ws) fh (genFrame ws) Horizontal

fillsWA :: Frame Number -> Frame Number -> V (Array Number) -> VVis Number
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

vPie :: Array (V (Array Number)) -> VVis Number
vPie xs =
  let mna = map (map (\a -> U.unsafeMaybe (F.minimum a))) (map plainVals xs)
      mn  = U.unsafeMaybe (F.minimum (map U.minimum mna))
      mxa = map (map (\a -> U.unsafeMaybe (F.maximum a))) (map plainVals xs)
      mx  = U.unsafeMaybe (F.maximum (map U.maximum mna))
      fh = Frame { frameMin: 0.0, frameMax: 1.0 }
      fw = Frame { frameMin: min 0.0 mn, frameMax: max 0.0 mx}
      fs = map (fillsWA fh fw) xs
  in MkPolar $ NextTo $ U.unsafeMaybe (NE.fromFoldable fs)

setW :: Number -> V Number -> V (Tuple Number Number)
setW w (Chc d l r) = Chc d (setW w l) (setW w r)
setW w (One h) = One (Tuple w h)

setH :: Number -> V Number -> V (Tuple Number Number)
setH h (Chc d l r) = Chc d (setH h l) (setH h r)
setH h (One w) = One (Tuple w h)

-- | An _unsafe_ helper function (when the parameter list is empty) that takes a
-- | list of variational numbers and produces a frame based on the minimum and
-- | maximum values.
genFrame :: L.List (V Number) -> Frame Number
genFrame vs =
  let vs'  = unsafePartial $ fromJust (NE.fromList vs)
      fmin = min (U.vminimum vs') 0.0
      fmax = max (U.vmaximum vs') 0.0
  in Frame { frameMax: fmax, frameMin: fmin }

fills ::
  L.List (V (Tuple Number Number)) ->
  Frame Number ->
  Frame Number ->
  Orientation ->
  NE.NonEmptyList (VVis Number)
fills vs fh fw o = U.unsafeNonEmpty $ map (genFill fh fw o) vs

-- | For a variational number, produce a `Fill` visualization.
genFill ::
  Frame Number ->
  Frame Number ->
  Orientation ->
  V (Tuple Number Number) ->
  VVis Number
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

hspace :: Number -> VVis Number
hspace v = spaceFill v 1.0 Horizontal

vspace :: Number -> VVis Number
vspace v = spaceFill 1.0 v Vertical

spaceFill :: Number -> Number -> Orientation -> VVis Number
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
nextTo' :: forall a. L.List (VVis a) -> VVis a
nextTo' vs = NextTo $ unsafePartial $ fromJust $ NE.fromList vs

nextTo :: forall a. Array (VVis a) -> VVis a
nextTo vs = nextTo' $ A.toUnfoldable vs

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `Above`.
above' :: forall a. L.List (VVis a) -> VVis a
above' vs = Above $ unsafePartial $ fromJust $ NE.fromList vs

-- | An _unsafe_ helper function (when the parameter array is empty) for
-- | composing with `Above`.
above :: forall a. Array (VVis a) -> VVis a
above vs = above' $ A.toUnfoldable vs

-- | Overlay the first visualization over the second
overlay :: forall a. VVis a -> VVis a -> VVis a
overlay v1 v2 = Overlay $ NE.cons v1 (NE.singleton v2)

overlayFlat :: forall a. VVis Number -> VVis Number -> VVis Number
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
setFrames :: forall a. Frame a -> Frame a -> VVis a -> VVis a
setFrames fh fw (Fill f) = Fill (f { frameH = fh, frameW = fw })
setFrames fh fw (V d l r) = V d (setFrames fh fw l) (setFrames fh fw r)
setFrames fh fw (NextTo vs) = NextTo $ map (setFrames fh fw) vs
setFrames fh fw (Above vs) = Above $ map (setFrames fh fw) vs
setFrames fh fw (MkCartesian v) = MkCartesian (setFrames fh fw v)
setFrames fh fw (MkPolar v) = MkPolar (setFrames fh fw v)
setFrames fh fw (Overlay vs) = Overlay $ map (setFrames fh fw) vs
setFrames fh fw (Stacked vs) = Stacked $ map (setFrames fh fw) vs

doStack :: VVis Number -> VVis Number -> VVis Number
doStack x (Stacked vs) = Stacked $ NE.cons x vs
doStack (V d1 l1 r1) (V d2 l2 r2) | d1 == d2 = V d1 (stack l1 r2) (stack r1 r2)
doStack (V d l r) v = V d (stack l v) (stack r v)
doStack v (V d l r) = V d (stack l v) (stack r v)
doStack (MkPolar x) y = MkPolar $ stack x y
doStack (MkCartesian x) y = MkCartesian $ stack x y
doStack x (MkPolar y) = MkPolar $ stack x y
doStack x (MkCartesian y) = MkCartesian $ stack x y
doStack x y = Stacked $ NE.cons x (NE.singleton y)

stack :: VVis Number -> VVis Number -> VVis Number
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

stacks :: Array Number -> Array Number -> NE.NonEmptyList (VVis Number)
stacks xs ys =
  let maxH = unsafePartial $ fromJust $ F.maximum $ A.zipWith (+) xs ys
      minH = unsafePartial $ fromJust $ F.minimum $ A.zipWith (+) xs ys
      fh = Frame { frameMin: min 0.0 minH, frameMax: max 0.0 maxH }
      fw = Frame { frameMin: 0.0, frameMax: 1.0 }
  in map (setFrames fh fw)
         (NE.zipWith stack2 (fillsH (map One xs)) (fillsH (map One ys)))

stack2 :: forall a. VVis a -> VVis a -> VVis a
stack2 x y = Stacked (NE.cons x (NE.singleton y))
