{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Musicology.Core.Helpers
  ( onoffGroups
  , groupsToPitches
  , groupsToSlices
  )
where

import           Musicology.Core

import qualified Data.List                     as L

onoffGroups :: (Foldable f, HasTime n) => f n -> [[OnOff n (TimeOf n)]]
onoffGroups notes = L.groupBy eq $ L.sortOn onset $ foldl convert mempty notes
 where
  convert onoffs note =
    Onset note (onset note) : Offset note (offset note) : onoffs
  eq a b = onset a == onset b

newPitches
  :: (Eq p, HasPitch a, IntervalOf a ~ p, HasTime a)
  => [Pitch p]
  -> [OnOff a (TimeOf a)]
  -> [Pitch p]
newPitches curr grp = (curr L.\\ (pitch <$> offs)) `L.union` (pitch <$> ons)
  where (ons, offs) = L.partition isOn grp

groupsToPitches
  :: (Foldable t, HasPitch a, HasTime a, Eq (IntervalOf a))
  => t [OnOff a (TimeOf a)]
  -> [[Pitch (IntervalOf a)]]
groupsToPitches groups = reverse $ case finalState of
  Nothing   -> finalAcc
  Just []   -> finalAcc
  Just last -> last : finalAcc
 where
  go (Nothing  , _  ) grp = (Just $ newPitches [] grp, [])
  go (Just curr, acc) grp = (Just $ newPitches curr grp, curr : acc)
  (finalState, finalAcc) = foldl go (Nothing, []) groups

groupsToSlices
  :: (Foldable t, HasTime n, HasPitch n, Eq (IntervalOf n))
  => t [OnOff n (TimeOf n)]
  -> [TimedEvent [Pitch (IntervalOf n)] (TimeOf n)]
groupsToSlices = reverse . snd . foldl go (Nothing, [])
 where
  halfSlice curr grp = (onset $ head grp, newPitches curr grp)
  go (Nothing           , _  ) grp = (Just $ halfSlice [] grp, [])
  go (Just (on, pitches), acc) grp = (Just nxt, slice : acc)
   where
    nxt   = halfSlice pitches grp
    slice = TimedEvent pitches on (fst nxt)
