{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Musicology.Polygrams where

import Musicology.Types
import Musicology.Grams
import Data.Machine ((~>), Process)
import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Random.MWC
import Musicology.Internal.Helpers (processFoldable)

-- debugging
import Musicology.IO.MidiFile

import qualified Streamly as SY
import qualified Streamly.Prelude as SY
import qualified Musicology.GramsStreamly as GSY
import System.IO.Unsafe (unsafePerformIO)

import qualified Streaming as SG
import qualified Streaming.Prelude as SG
import qualified Musicology.GramsStreaming as GSG

onsetdist :: HasTime a => a -> a -> TimeOf a
onsetdist x y = onset y - onset x

vonset (n:_) = onset n

voffset vert = maximum $ offset <$> vert

nooverlap (v2:v1:_) = vonset v2 >= voffset v1

-- groupdiff v1 v2 = vonset v2 - vonset v1

groupdist :: HasTime a => TimeOf a -> [a] -> [a] -> TimeOf a
groupdist k v1 v2 = 
  let k1 = k+1
  in if vonset v2 - vonset v1 > k
     then k1
     else 0

verticals :: (HasTime a, Foldable t, Integral n) =>
              t a -> n -> TimeOf a -> [[a]]
verticals notes = skipgramsS notes (const True) onsetdist

verticalsR :: (HasTime a, Foldable t, Integral n, PrimMonad m) =>
             t a -> Double -> Gen (PrimState m) -> n -> TimeOf a -> m [[a]]
verticalsR notes p gen =
  skipgramsRS notes p gen (const True) onsetdist

horizontals :: (HasTime a, Foldable t, Integral n) =>
               t [a] -> n -> TimeOf a -> [[[a]]]
horizontals verts n k = skipgrams verts nooverlap (groupdist k) n k

horizontalsR :: (HasTime a, Foldable t, Integral n, PrimMonad m) =>
                t [a] -> Double -> Gen (PrimState m) -> n -> TimeOf a -> m [[[a]]]
horizontalsR verts p gen n k =
  skipgramsR verts p gen nooverlap (groupdist k) n k

polygrams :: (HasTime a, Foldable t, Integral n) =>
             n -> n -> TimeOf a -> t a -> [[[a]]]
polygrams nv ns barlen notes =
  horizontals verts ns barlen
  where verts = verticals notes nv barlen

polygramsR :: (HasTime a, Foldable t, Integral n, PrimMonad m) =>
             t a -> Double -> Double -> Gen (PrimState m) -> n -> n -> TimeOf a -> m [[[a]]]
polygramsR notes pv ps gen nv ns barlen = do
  verts <- verticalsR notes pv gen nv barlen
  horizontalsR verts ps gen ns barlen

-- with machines

verticalsMach coin = skipgramsRSMach coin (const True) onsetdist

horizontalsMach coin n k = skipgramsRMach coin nooverlap (groupdist k) n k

polygramsMachR cv cs nv ns kv ks = verticalsMach cv nv kv ~> horizontalsMach cs ns ks

polygramsMach :: (HasTime a, Integral n) =>
              n -> n -> TimeOf a -> TimeOf a -> Process a [[a]]
polygramsMach = polygramsMachR (return True) (return True)

polygrams' :: (HasTime a, Foldable t, Integral n) =>
              n -> n -> TimeOf a -> t a -> [[[a]]]
polygrams' nv ns barlen notes = processFoldable notes $ polygramsMach nv ns barlen barlen

-- with streamly

verticalsStreamly coin = GSY.skipgramsRS coin (const True) onsetdist

horizontalsStreamly coin n k = GSY.skipgramsR coin nooverlap (groupdist k) n k

polygramsStreamlyR cv cs nv ns kv ks = horizontalsStreamly cs ns ks . verticalsStreamly cv nv kv

polygramsStreamly :: (HasTime a, Integral n, SY.MonadAsync m, SY.IsStream t) =>
                n -> n -> TimeOf a -> TimeOf a -> t m a -> t m [[a]]
polygramsStreamly = polygramsStreamlyR (return True) (return True)

polygrams'' :: (HasTime a, Foldable t, Integral n) =>
               n -> n -> TimeOf a -> t a -> [[[a]]]
polygrams'' nv ns barlen = unsafePerformIO . SY.toList . polygramsStreamly nv ns barlen barlen . SY.fromFoldable

verticals'' :: (HasTime a, Foldable t, Integral n) =>
               n -> (TimeOf a) -> t a -> [[a]]
verticals'' nv barlen = unsafePerformIO . SY.toList . verticalsStreamly (return True) nv barlen . SY.fromFoldable

-- with streaming

verticalsStreaming coin = GSG.skipgramsRS coin (const True) onsetdist

horizontalsStreaming coin n k = GSG.skipgramsR coin nooverlap (groupdist k) n k

polygramsStreamingR cv cs nv ns kv ks = horizontalsStreaming cs ns ks . verticalsStreaming cv nv kv

polygramsStreaming :: (HasTime a, Integral n, Monad m) =>
                n -> n -> TimeOf a -> TimeOf a -> SG.Stream (SG.Of a) m r -> SG.Stream (SG.Of [[a]]) m r
polygramsStreaming = polygramsStreamingR (return True) (return True)

polygrams''' :: (HasTime a, Foldable t, Integral n) =>
               n -> n -> TimeOf a -> t a -> [[[a]]]
polygrams''' nv ns barlen = SG.runIdentity . SG.toList_ . polygramsStreaming nv ns barlen barlen . SG.each

verticals''' :: (HasTime a, Foldable t, Integral n) =>
               n -> (TimeOf a) -> t a -> [[a]]
verticals''' nv barlen = SG.runIdentity . SG.toList_ . verticalsStreaming (return True) nv barlen . SG.each
