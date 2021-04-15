{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Musicology.Core.Slicing
  ( onOffGroups
  , groupsToSlices
  , slicePiece
  , Slicer(..)
  , timed
  , cleanSlicer
  , tiedSlicer
  )
where

import           Musicology.Core

import qualified Data.List                     as L
import           Lens.Micro.Extras              ( view )

onOffGroups :: (Foldable f, HasTime n) => f n -> [[OnOff n (TimeOf n)]]
onOffGroups notes = L.groupBy eq $ L.sortOn onset $ foldl convert mempty notes
 where
  convert onoffs note =
    Onset note (onset note) : Offset note (offset note) : onoffs
  eq a b = onset a == onset b

data Slicer a t st s = Slicer
  { slInit :: [OnOff a t] -> st
  , slNext :: st -> [OnOff a t] -> (st, s)
  , slFinal :: st -> Maybe s
  }

-- newPitches
--   :: (Eq p, HasPitch a, IntervalOf a ~ p)
--   => [Pitch p]
--   -> [OnOff a (TimeOf a)]
--   -> [Pitch p]

groupsToSlices :: Foldable f => Slicer a t st s -> f [OnOff a t] -> [s]
groupsToSlices (Slicer init next cleanUp) groups =
  reverse $ case foldl nextSlice Nothing groups of
    Nothing        -> []
    Just (st, acc) -> case cleanUp st of
      Just rest -> rest : acc
      Nothing   -> acc
 where
  nextSlice Nothing          grp = Just (init grp, [])
  nextSlice (Just (st, acc)) grp = Just (st', slice' : acc)
    where (st', slice') = next st grp

slicePiece :: (Foldable f, HasTime n) => Slicer n (TimeOf n) st s -> f n -> [s]
slicePiece slicer notes = groupsToSlices slicer $ onOffGroups notes

timed (Slicer init next cleanUp) = Slicer init' next' cleanUp'
 where
  time = onset . head
  init' grp = (time grp, init grp)
  next' (on, st) grp = ((onset $ head grp, st'), timedSlice)
   where
    (slice, st') = next st grp
    timedSlice   = TimedEvent slice on (time grp)
  cleanUp' (on, st) = case cleanUp st of
    Nothing    -> Nothing
    Just slice -> Just $ TimedEvent slice on on

cleanSlicer :: (Eq a) => Slicer a t [a] [a]
cleanSlicer = Slicer init next cleanUp
 where
  contents onoff = fmap (view onOffContent) onoff
  newPitches curr grp = (curr L.\\ contents offs) `L.union` contents ons
    where (ons, offs) = L.partition isOn grp
  init = contents . filter isOn
  next curr grp = (newPitches curr grp, curr)
  cleanUp []   = Nothing
  cleanUp last = Just last

-- groupsToPitches
--   :: (Foldable f, HasPitch a, Eq (IntervalOf a))
--   => f [OnOff a t]
--   -> [[Pitch (IntervalOf a)]]
-- groupsToPitches = groupsToAny pitchSlicer

tiedSlicer :: (Eq a) => Slicer a t ([a], [a]) [(a, Tied)]
tiedSlicer = Slicer init next cleanUp
 where
  content = view onOffContent
  init grp = (content <$> filter isOn grp, [])
  next (currNew, currOld) grp = ((currNew', currOld'), notes)
   where
    (onEvs, offEvs) = L.partition isOn grp
    ons             = content <$> onEvs
    offs            = content <$> offEvs
    singles         = (, Single) <$> currNew `L.intersect` offs
    starts          = (, Starts) <$> currNew L.\\ offs
    continues       = (, Continues) <$> currOld L.\\ offs
    stops           = (, Stops) <$> currOld `L.intersect` offs
    notes           = singles <> starts <> continues <> stops
    currOld'        = (currNew <> currOld) L.\\ offs
    currNew'        = ons L.\\ currOld'
  cleanUp ([] , [] ) = Nothing
  cleanUp (new, old) = Just $ fmap (, Single) new <> fmap (, Stops) old

-- -- groupsToTied
-- --   :: (Foldable t, HasTime n, Eq n) => t [OnOff n (TimeOf n)] -> [[(n, Tied)]]
-- groupsToTied :: (Foldable f, Eq a) => f [OnOff a t] -> [[(a, Tied)]]
-- groupsToTied = groupsToAny tiedSlicer
