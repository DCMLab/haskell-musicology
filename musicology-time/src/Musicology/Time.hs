module Musicology.Time where

import           Data.Ratio
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Fixed                     ( mod' )

data MBS = MBS Int Int (Ratio Int)
  deriving (Eq, Ord)

instance Show MBS where
  show (MBS m b s) = show (m + 1) <> "." <> show (b + 1) <> "." <> showFrac s
   where
    showFrac 0 = "0"
    showFrac f = show (numerator f) <> "/" <> show (denominator f)

data TimeSignature = TS {tsNum :: Int, tsDenom :: Int}
  deriving (Eq, Ord, Show)

measureDuration :: TimeSignature -> Ratio Int
measureDuration (TS num denom) = num % denom

newtype Meter = MeterGroups [Int]

defaultMeter :: TimeSignature -> Meter
defaultMeter (TS num denom)
  | num == 1         = MeterGroups [1]
  | num `mod` 3 == 0 = MeterGroups $ replicate (num `div` 3) 3
  | even num         = MeterGroups $ replicate (num `div` 2) 2
  | otherwise        = MeterGroups $ replicate num 1

timeToMBS :: [(Ratio Int, TimeSignature)] -> Ratio Int -> MBS
timeToMBS sigs t = go (0 :: Int)
                      (fromMaybe (0 % 1, TS 4 4) (listToMaybe sigs))
                      sigs
 where
  go :: Int -> (Ratio Int, TimeSignature) -> [(Ratio Int, TimeSignature)] -> MBS
  go bars (currOn, currSig) [] = complete bars currOn currSig
  go bars (currOn, currSig) ((nextOn, nextSig) : rest) = if t < nextOn
    then complete bars currOn currSig
    else go (ceiling $ (nextOn - currOn) / measureDuration currSig)
            (nextOn, nextSig)
            rest
  complete bars on sig = MBS measure beat subb
   where
    barlen  = measureDuration sig
    beatlen = 1 % tsDenom sig
    reltime = t - on
    relbar  = floor (reltime / barlen)
    inbar   = reltime `mod'` barlen
    measure = relbar + bars
    beat    = floor (inbar / beatlen)
    subb    = (inbar / beatlen) `mod'` 1
