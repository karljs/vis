module Vis
  ( module Vis.Types

  , color
  , color1
  , flop
  , removeCoord
  , reorient
  , rotate

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
  ) where

import Color (Color, white)
import Color.Scheme.MaterialDesign (green)
import Data.Array as A
import Data.Foldable as F
import Data.List (List(Nil), concatMap, filter, nub, partition, (:)) as L
import Data.List.NonEmpty (NonEmptyList, cons, fromList, head, singleton, toList, zipWith) as NE
import Data.Map (empty)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String (take)
import Data.Tuple (Tuple(..))
import Math (min)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, flip, map, max, show, ($), (+), (<), (<<<), (<>), (||))
import Util (doUnsafeListOp, intersperse, maximum, maybe1, minimum, unsafeNonEmpty, vmaximum, vminimum)
import V (Decision, Dim, Dir(..), V(..), lookupDim)
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
reorient (NextTo v) = NextTo (v { vs = map reorient v.vs })
reorient (Above v) = Above (v { vs = map reorient v.vs })
reorient (MkCartesian v) = MkCartesian (reorient v)
reorient (MkPolar v) = MkPolar (reorient v)
reorient (Overlay v) = Overlay (v { vs = map reorient v.vs })
reorient (Stacked v) = Stacked (v { vs = map reorient v.vs })

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
flop (NextTo v) = Above (v { vs = map flop v.vs })
flop (Above v) = NextTo (v { vs = map flop v.vs })
flop (MkCartesian v) = MkCartesian (flop v)
flop (MkPolar v) = MkPolar (flop v)
flop (Overlay v) = Overlay (v { vs = map flop v.vs })
flop (Stacked v) = Stacked (v { vs = map flop v.vs })

-- | Flop, then reorient
rotate :: forall a. VVis a -> VVis a
rotate = reorient <<< flop

-- | Iterate over a visualization and remove all the constructors that change
-- | the coordinate system.
removeCoord :: forall a. VVis a -> VVis a
removeCoord (Fill f) = Fill f
removeCoord (V d l r) = V d (removeCoord l) (removeCoord r)
removeCoord (NextTo v) = NextTo (v { vs = map removeCoord v.vs })
removeCoord (Above v) = Above (v { vs = map removeCoord v.vs })
removeCoord (MkCartesian v) = removeCoord v
removeCoord (MkPolar v) = removeCoord v
removeCoord (Overlay v) = Overlay (v { vs = map removeCoord v.vs })
removeCoord (Stacked v) = Stacked (v { vs = map removeCoord v.vs })

space :: VVis Number -> Number -> VVis Number
space (NextTo v) n =
  let newvs = doUnsafeListOp (intersperse (hspace n)) v.vs
  in NextTo (v { vs = newvs })
space (Above v) n =
  let newvs = doUnsafeListOp (intersperse (vspace n)) v.vs
  in Above (v { vs = newvs })
space (V d l r) n = V d (space l n) (space r n)
space (MkCartesian v) n = MkCartesian (space v n)
space (MkPolar v) n = MkPolar (space v n)
space (Overlay v) n =
  let newvs = doUnsafeListOp (intersperse (vspace n)) v.vs
  in Overlay (v { vs = newvs })
space (Stacked v) _ = Stacked v
space (Fill v) _ = Fill v

leftSpace :: VVis Number -> Number -> VVis Number
leftSpace v n = NextTo { vs: (NE.cons (hspace n) (NE.singleton v))
                       }

rightSpace :: VVis Number -> Number -> VVis Number
rightSpace v n = NextTo { vs: (NE.cons v (NE.singleton (hspace n)))
                        }

topSpace :: VVis Number -> Number -> VVis Number
topSpace v n = Above { vs: (NE.cons (vspace n) (NE.singleton v))
                     }

bottomSpace :: VVis Number -> Number -> VVis Number
bottomSpace v n = Above { vs: (NE.cons v (NE.singleton (vspace n)))
                        }


--------------------------------------------------------------------------------
-- Aesthetics and style functions

color :: forall a. VVis a -> NE.NonEmptyList Color -> VVis a
color (Fill f) cs = let (VPs vps) = f.vps
                    in Fill (f { vps = (VPs (vps { color = NE.head cs } )) })
color (V d l r) cs = V d (color l cs) (color r cs)
color (NextTo v) cs = NextTo (v { vs = NE.zipWith color1 v.vs cs })
color (Above v) cs = Above (v { vs = NE.zipWith color1 v.vs cs })
color (MkCartesian v) cs = MkCartesian (color v cs)
color (MkPolar v) cs = MkPolar (color v cs)
color (Overlay v) cs = Overlay (v { vs = NE.zipWith color1 v.vs cs })
color (Stacked v) cs = Stacked (v { vs = NE.zipWith color1 v.vs cs })

color1 :: forall a. VVis a -> Color -> VVis a
color1 (Fill f) c = let (VPs vps) = f.vps
                    in Fill (f { vps = (VPs (vps { color = c } )) })
color1 (V d l r) c = V d (color1 l c) (color1 r c)
color1 (NextTo v) c = NextTo (v { vs = map (flip color1 c) v.vs })
color1 (Above v) c = Above (v { vs = map (flip color1 c) v.vs })
color1 (MkCartesian v) c = MkCartesian (color1 v c)
color1 (MkPolar v) c = MkPolar (color1 v c)
color1 (Overlay v) c = Overlay (v { vs = map (flip color1 c) v.vs })
color1 (Stacked v) c = Stacked (v { vs = map (flip color1 c) v.vs })

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
selectVis dec (NextTo v) = NextTo (v { vs = map (selectVis dec) v.vs })
selectVis dec (Above v) = Above (v { vs = map (selectVis dec) v.vs })
selectVis dec (MkCartesian v) = MkCartesian (selectVis dec v)
selectVis dec (MkPolar v) = MkPolar (selectVis dec v)
selectVis dec (Overlay v) = Overlay (v { vs = map (selectVis dec) v.vs })
selectVis dec (Stacked v) = Stacked (v { vs = map (selectVis dec) v.vs })

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
  visDimsHelp (NextTo v) = L.concatMap visDimsHelp (NE.toList v.vs)
  visDimsHelp (Above v) = L.concatMap visDimsHelp (NE.toList v.vs)
  visDimsHelp (MkCartesian v) = visDimsHelp v
  visDimsHelp (MkPolar v) = visDimsHelp v
  visDimsHelp (Overlay v) = L.concatMap visDimsHelp (NE.toList v.vs)
  visDimsHelp (Stacked v) = L.concatMap visDimsHelp (NE.toList v.vs)

--------------------------------------------------------------------------------
-- Queries

isVisible :: forall e. { vps :: VPs | e } -> Boolean
isVisible r = let (VPs vps) = r.vps
              in vps.visible

getHeight :: VPs -> Number
getHeight (VPs vps) = maybe1 vps.height

getWidth :: VPs -> Number
getWidth (VPs vps) = maybe1 vps.width

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
  in (maybe false (\h -> h < 0.0) vps.height) ||
     (maybe false (\w -> w < 0.0) vps.width)
anyNegative _ = false

isFill :: forall a. VVis a -> Boolean
isFill (Fill _) = true
isFill _ = false

visOrientation :: forall a. VVis a -> Orientation
visOrientation (Fill f) = getOrientation f.vps
visOrientation (V _ l _) = visOrientation l
visOrientation (NextTo v) = visOrientation (NE.head v.vs)
visOrientation (Above v) = visOrientation (NE.head v.vs)
visOrientation (MkCartesian v) = visOrientation v
visOrientation (MkPolar v) = visOrientation v
visOrientation (Overlay v) = visOrientation (NE.head v.vs)
visOrientation (Stacked v) = visOrientation (NE.head v.vs)


visMaxH :: VVis Number -> Number
visMaxH (Fill f) = getHeight f.vps
visMaxH (V d l r) = max (visMaxH l) (visMaxH r)
visMaxH (NextTo v) = maximum (map visMaxH v.vs)
visMaxH (Above v) = maximum (map visMaxH v.vs)
visMaxH (MkCartesian v) = visMaxH v
visMaxH (MkPolar v) = visMaxH v
visMaxH (Overlay v) = maximum (map visMaxH v.vs)
visMaxH (Stacked v) = case visOrientation (NE.head v.vs) of
  Vertical -> F.foldr (+) 0.0 (map visMaxH v.vs)
  Horizontal -> maximum (map visMaxH v.vs)

visMinH :: VVis Number -> Number
visMinH (Fill f) = getHeight f.vps
visMinH (V d l r) = min (visMinH l) (visMinH r)
visMinH (NextTo v) = minimum (map visMinH v.vs)
visMinH (Above v) = minimum (map visMinH v.vs)
visMinH (MkCartesian v) = visMinH v
visMinH (MkPolar v) = visMinH v
visMinH (Overlay v) = minimum (map visMinH v.vs)
visMinH (Stacked v) = case visOrientation (NE.head v.vs) of
  Vertical -> F.foldr (+) 0.0 (map visMinH v.vs)
  Horizontal -> minimum (map visMinH v.vs)

visMaxW :: VVis Number -> Number
visMaxW (Fill f) = getWidth f.vps
visMaxW (V d l r) = max (visMaxW l) (visMaxW r)
visMaxW (NextTo v) = maximum (map visMaxW v.vs)
visMaxW (Above v) = maximum (map visMaxW v.vs)
visMaxW (MkCartesian v) = visMaxW v
visMaxW (MkPolar v) = visMaxW v
visMaxW (Overlay v) = maximum (map visMaxW v.vs)
visMaxW (Stacked v) = case visOrientation (NE.head v.vs) of
  Horizontal -> F.foldr (+) 0.0 (map visMaxW v.vs)
  Vertical -> maximum (map visMaxW v.vs)

visMinW :: VVis Number -> Number
visMinW (Fill f) = getWidth f.vps
visMinW (V d l r) = min (visMinW l) (visMinW r)
visMinW (NextTo v) = minimum (map visMinW v.vs)
visMinW (Above v) = minimum (map visMinW v.vs)
visMinW (MkCartesian v) = visMinW v
visMinW (MkPolar v) = visMinW v
visMinW (Overlay v) = minimum (map visMinW v.vs)
visMinW (Stacked v) = case visOrientation (NE.head v.vs) of
  Horizontal -> F.foldr (+) 0.0 (map visMinW v.vs)
  Vertical -> minimum (map visMinW v.vs)

--------------------------------------------------------------------------------
-- Helper functions for (mostly unsafely) constructing nonempty lists of things

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
      fmin = min (vminimum vs') 0.0
      fmax = max (vmaximum vs') 0.0
  in Frame { frameMax: fmax, frameMin: fmin }

fills ::
  L.List (V (Tuple Number Number)) ->
  Frame Number ->
  Frame Number ->
  Orientation ->
  NE.NonEmptyList (VVis Number)
fills vs fh fw o = unsafeNonEmpty $ map (genFill fh fw o) vs

-- | For a variational number, produce a `Fill` visualization.
genFill ::
  Frame Number ->
  Frame Number ->
  Orientation ->
  V (Tuple Number Number) ->
  VVis Number
genFill fh fw o (One (Tuple w h)) =
  let vp = VPs { height: Just h
               , width: Just w
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
          , label: Just l
          }
genFill fh fw o (Chc d l r) = V d (genFill fh fw o l) (genFill fh fw o r)

hspace :: Number -> VVis Number
hspace v = spaceFill (Just v) Nothing Horizontal

vspace :: Number -> VVis Number
vspace v = spaceFill Nothing (Just v) Vertical

spaceFill :: Maybe Number -> Maybe Number -> Orientation -> VVis Number
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
                       , size: 36.0 }

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `NextTo`.
nextTo' :: forall a. L.List (VVis a) -> VVis a
nextTo' vs = NextTo { vs: unsafePartial $ fromJust $ NE.fromList vs
                    }

nextTo :: forall a. Array (VVis a) -> VVis a
nextTo vs = nextTo' $ A.toUnfoldable vs

-- | An _unsafe_ helper function (when the parameter list is empty) for
-- | composing with `Above`.
above' :: forall a. L.List (VVis a) -> VVis a
above' vs = Above { vs: unsafePartial $ fromJust $ NE.fromList vs
                  }

-- | An _unsafe_ helper function (when the parameter array is empty) for
-- | composing with `Above`.
above :: forall a. Array (VVis a) -> VVis a
above vs = above' $ A.toUnfoldable vs


-- | Overlay the first visualization over the second
overlay :: forall a. VVis a -> VVis a -> VVis a
overlay v1 v2 = Overlay { vs: NE.cons v1 (NE.singleton v2) }

overlayFlat :: forall a. VVis Number -> VVis Number -> VVis Number
overlayFlat v1 v2 =
  let maxH = max (visMaxH v1) (visMaxH v2)
      maxW = max (visMaxW v1) (visMaxW v2)
      minH = min (visMinH v1) (visMinH v2)
      minW = min (visMinW v1) (visMinW v2)
      v1f = setFrames (Frame { frameMin: minH, frameMax: maxH })
                      (Frame { frameMin: minW, frameMax: maxW })
                      v1
      v2f = setFrames (Frame { frameMin: minH, frameMax: maxH })
                      (Frame { frameMin: minW, frameMax: maxW })
                      v2
  in Overlay { vs: NE.cons v1f (NE.singleton v2f) }

-- | Set or overwrite the frames for an existing visualization.  Parameters
-- | have the height frame first, followed by the width frame.
setFrames :: forall a. Frame a -> Frame a -> VVis a -> VVis a
setFrames fh fw (Fill f) = Fill (f { frameH = fh, frameW = fw })
setFrames fh fw (V d l r) = V d (setFrames fh fw l) (setFrames fh fw r)
setFrames fh fw (NextTo v) = NextTo (v { vs = map (setFrames fh fw) v.vs })
setFrames fh fw (Above v) = Above (v { vs = map (setFrames fh fw) v.vs })
setFrames fh fw (MkCartesian v) = MkCartesian (setFrames fh fw v)
setFrames fh fw (MkPolar v) = MkPolar (setFrames fh fw v)
setFrames fh fw (Overlay v) = Overlay (v { vs = map (setFrames fh fw) v.vs })
setFrames fh fw (Stacked v) = Stacked (v { vs = map (setFrames fh fw) v.vs })

doStack :: forall a. VVis a -> VVis a -> VVis a
doStack x (Stacked v) = Stacked (v {vs = NE.cons x v.vs })
doStack (V d l r) v = V d (doStack l v) (doStack r v)
doStack v (V d l r) = V d (doStack l v) (doStack r v)
doStack x y = Stacked { vs: NE.cons x (NE.singleton y) }

stack :: VVis Number -> VVis Number -> VVis Number
stack v1 v2 =
  let v3 = stackHelp v1 v2
      fh = Frame { frameMin: min 0.0 (visMinH v3)
                 , frameMax: max 0.0 (visMaxH v3) }
      fw = Frame { frameMin: min 0.0 (visMinW v3)
                 , frameMax: max 0.0 (visMaxW v3) }
  in setFrames fh fw v3 where
    stackHelp (NextTo v1) (NextTo v2) =
      NextTo { vs: (NE.zipWith doStack v1.vs v2.vs) }
    stackHelp (Above v1) (Above v2) =
      Above { vs: (NE.zipWith doStack v1.vs v2.vs) }
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
stack2 x y = Stacked { vs: NE.cons x (NE.singleton y) }
