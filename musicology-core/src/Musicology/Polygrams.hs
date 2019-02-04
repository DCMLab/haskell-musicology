{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Musicology.Polygrams where

import Musicology.Types
import Musicology.Grams
import Data.Machine ((~>), Process)
import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Random.MWC
import Musicology.Internal.Helpers (processFoldable)

-- debugging
import Musicology.IO.MidiFile

onsetdist :: Timed a => a -> a -> TimeOf a
onsetdist x y = onset y - onset x

vonset (n:_) = onset n

voffset vert = maximum $ offset <$> vert

nooverlap (v2:v1:_) = vonset v2 >= voffset v1

-- groupdiff v1 v2 = vonset v2 - vonset v1

groupdist :: Timed a => TimeOf a -> [a] -> [a] -> TimeOf a
groupdist k v1 v2 = 
  let k1 = k+1
  in if vonset v2 - vonset v1 > k
     then k1
     else 0

verticals :: (Timed a, Foldable t, Integral n) =>
              t a -> n -> TimeOf a -> [[a]]
verticals notes = skipgramsS notes (const True) onsetdist

verticalsR :: (Timed a, Foldable t, Integral n, PrimMonad m) =>
             t a -> Double -> Gen (PrimState m) -> n -> TimeOf a -> m [[a]]
verticalsR notes p gen n k =
  skipgramsRS notes p gen (const True) onsetdist n k

horizontals :: (Timed a, Foldable t, Integral n) =>
               t [a] -> n -> TimeOf a -> [[[a]]]
horizontals verts n k = skipgrams verts nooverlap (groupdist k) n k

horizontalsR :: (Timed a, Foldable t, Integral n, PrimMonad m) =>
                t [a] -> Double -> Gen (PrimState m) -> n -> TimeOf a -> m [[[a]]]
horizontalsR verts p gen n k =
  skipgramsR verts p gen nooverlap (groupdist k) n k

polygrams :: (Timed a, Foldable t, Integral n) =>
             t a -> n -> n -> TimeOf a -> [[[a]]]
polygrams notes nv ns barlen =
  horizontals verts ns barlen
  where verts = verticals notes nv barlen

polygramsR :: (Timed a, Foldable t, Integral n, PrimMonad m) =>
             t a -> Double -> Double -> Gen (PrimState m) -> n -> n -> TimeOf a -> m [[[a]]]
polygramsR notes pv ps gen nv ns barlen = do
  verts <- verticalsR notes pv gen nv barlen
  horizontalsR verts ps gen ns barlen

-- with machines

verticalsMach coin = skipgramsRSMach coin (const True) onsetdist

horizontalsMach coin n k = skipgramsRMach coin nooverlap (groupdist k) n k

polygramsMachR cv cs nv ns kv ks = verticalsMach cv nv kv ~> horizontalsMach cs ns ks

polygramsMach :: (Timed a, Integral n) =>
              n -> n -> TimeOf a -> TimeOf a -> Process a [[a]]
polygramsMach = polygramsMachR (return True) (return True)

polygrams' :: (Timed a, Foldable t, Integral n) =>
             t a -> n -> n -> TimeOf a -> [[[a]]]
polygrams' notes nv ns barlen = processFoldable notes $ polygramsMach nv ns barlen barlen
