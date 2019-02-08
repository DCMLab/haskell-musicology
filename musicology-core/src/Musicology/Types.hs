{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Musicology.Types
  ( IntervalClass(..)
  , Interval(..)
  , oct, iabs
  -- , ClassyInterval(..)
  , Diatonic(..), Chromatic(..)
  , aug, dim, down
  , VectorSpace(..), AdditiveGroup(..)
  , MidiInterval, MidiIC(..), mic
  , SInterval(..), SIC(..), sic
  , unison, second, third, fourth, tritone, fifth, sixth, seventh
  , unison', second', third', fourth', tritone', fifth', sixth', seventh'
  , Pitch(..), toPitch, toInterval, pto, pfrom
  , MidiPitch, MidiPC, SPitch, SPC
  , flt, shp, nat
  , c, d, e, f, g, a, b
  , c', d', e', f', g', a', b'
  , Timed(..), HasTime(..)
  , Pitched(..), HasInterval(..), HasPitch(..)
  -- , PitchClassContainer(..), PitchContainer(..), embed, embed'
  -- , AbsPitchContainer(..)
  , TimedEvent(..)
  , Note(..)
  , OnOff(..), isOn, isOff
  ) where

import Data.Monoid ((<>))
import Data.Functor.Identity (Identity(..))

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Data.VectorSpace

import qualified Euterpea.Music as EM
import Data.Aeson

---------------
-- Intervals --
---------------

-- should pitches always be nums?
-- does multiplication always make sense?
-- scalar multiplication? vector space?

class (Interval i, Interval (IOf i), VectorSpace i, ICOf (IOf i) ~ i) => IntervalClass i where
  type IOf i
--  ic :: IOf i -> i
  emb :: i -> IOf i

class VectorSpace i => Interval i where
  type ICOf i
  ic :: i -> ICOf i
  toMidi :: i -> MidiInterval
  toFreq :: i -> Double
  toFreq i = 440 * 2 ** ((fromIntegral (toMidi i) - 9) / 12)
  octave :: Int -> i
  direction :: i -> Ordering
  default direction :: Ord i => i -> Ordering
  direction i = compare i zeroV

oct :: IntervalClass i => Int -> i -> IOf i
oct octs ic = emb ic ^+^ octave octs

iabs :: Interval i => i -> i
iabs i | direction i == LT = negateV i
       | otherwise         = i

-- class (Interval i, IntervalClass (PCOf i), IOf (ICOf i) ~ i) => ClassyInterval i where  

class Interval i => Diatonic i where
  isStep :: i -> Bool

class Interval i => Chromatic i where
  chromaticSemitone :: i

dim :: Chromatic i => i -> i
dim = (^-^chromaticSemitone)

aug :: Chromatic i => i -> i
aug = (^+^chromaticSemitone)

-- MidiInterval
---------------

newtype MidiIC = MidiIC Int
  deriving (Eq, Ord, NFData)

mic :: Int -> MidiIC
mic = MidiIC . flip mod 12

instance Show MidiIC where
  show (MidiIC i) = show i

instance Num MidiIC where
  fromInteger = mic . fromInteger
  (MidiIC a) + (MidiIC b) = mic $ a+b
  (MidiIC a) - (MidiIC b) = mic $ a-b
  (MidiIC a) * (MidiIC b) = mic $ a*b -- never use this one!
  negate (MidiIC i) = mic (-i)
  abs = id
  signum (MidiIC i) = mic $ signum i

instance AdditiveGroup MidiIC where
  zeroV = MidiIC 0
  negateV = negate
  (^+^) = (+)
  (^-^) = (-)

instance VectorSpace MidiIC where
  type Scalar MidiIC = Int
  s *^ (MidiIC i) = mic (s*i)

instance Interval MidiIC where
  type ICOf MidiIC = MidiIC
  ic = id
  toMidi (MidiIC i) = i+60
  octave x = 0
  direction (MidiIC 0) = EQ
  direction (MidiIC i) = if i == 0
                         then EQ
                         else compare 6 i

instance IntervalClass MidiIC where
  type IOf MidiIC = MidiInterval
  -- ic = mic
  emb (MidiIC i) = i

instance Diatonic MidiIC where
  isStep (MidiIC i) = i <= 2 || i < 12 && i > 9 -- no i < 12?

instance Chromatic MidiIC where
  chromaticSemitone = MidiIC 1

instance EM.ToMusic1 (MidiIC) where
  toMusic1 = EM.toMusic1 . EM.mMap (\i -> (emb i + 60))

type MidiInterval = Int
--  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance Interval Int where
  type ICOf Int = MidiIC
  ic = mic
  toMidi = id
  toFreq i = 440 * 2 ** ((fromIntegral i - 69)/12)
  octave = (12*)
--  direction i = compare i 0
--  icInt int = if int > 6 then int - 12 else int

-- instance ClassyInterval Int where
--   type PCOf Int = MidiIC

instance Diatonic Int where
  isStep a = (abs a) <= 2

instance Chromatic Int where
  chromaticSemitone = 1

-- Spelled Interval
-------------------

-- data NoteNames = A | B | C | D | E | F | G
--   deriving (Eq, Ord, Show)

-- spelled pitch type and instances

diachrom = [0, 2, 4, 5, 7, 9, 11]
dia2chrom d = (diachrom !! (mod d 7)) + 12 * (div d 7)
dianames = ["c", "d", "e", "f", "g", "a", "b"]
diaints  = ["uni", "2nd", "3rd", "4th", "5th", "6th", "7th"]
diaget lst  = (lst!!) . (flip mod 7)
accstr 0 _ _ = ""
accstr n u d | n > 0     = take n $ repeat u
             | otherwise = take (abs n) $ repeat d

data SInterval = SInterval
                 { dSteps :: Int
                 , cSteps :: Int
                 }
  deriving (Ord, Eq, Generic)

down x = negateV x

minor int = int 0
major int = int 1

unison    = SInterval 0 0
second  x = SInterval 1 (1+x)
third   x = SInterval 2 (3+x)
fourth    = SInterval 3 5
tritone   = aug fourth -- SInterval 3 6
fifth     = SInterval 4 7
sixth   x = SInterval 5 (8+x)
seventh x = SInterval 6 (10+x)

instance NFData SInterval

instance Show SInterval where
  show (SInterval d c) = diaget diaints d <> accstr augs '+' '-' <> show (div d 7)
    where augs = c - dia2chrom d

instance AdditiveGroup SInterval where
  zeroV = SInterval 0 0
  negateV (SInterval d c) = SInterval (-d) (-c)
  (SInterval d1 c1) ^+^ (SInterval d2 c2) = SInterval (d1+d2) (c1+c2)
  (SInterval d1 c1) ^-^ (SInterval d2 c2) = SInterval (d1-d2) (c1-c2)

instance VectorSpace SInterval where
  type Scalar SInterval = Int
  s *^ (SInterval d c) = SInterval (s*d) (s*c)

instance Interval SInterval where
  type ICOf SInterval = SIC
  ic (SInterval d c) = sic d c
  toMidi (SInterval d c) = c + 12
  octave n = SInterval (7*n) (12*n)
--  direction (SInterval d _) = compare d 0 -- is this a good idea?

-- instance ClassyInterval SInterval where
--   type ICOf SInterval = SIC

instance Diatonic SInterval where
  isStep (SInterval d _) = abs d < 2

instance Chromatic SInterval where
  chromaticSemitone = SInterval 0 1

instance EM.ToMusic1 (SInterval) where
  toMusic1 = EM.toMusic1 . EM.mMap toMidi

-- spelled pitch class type and instances

data SIC = SIC Int Int
  deriving (Ord, Eq, Generic)

sic d c = SIC (mod d 7) (mod c 12)

unison'    = sic 0 0
second'  x = sic 1 (1+x)
third'   x = sic 2 (3+x)
fourth'    = sic 3 5
tritone'   = aug fourth' -- sic 3 6
fifth'     = sic 4 7
sixth'   x = sic 5 (8+x)
seventh' x = sic 6 (10+x)

instance NFData SIC

instance Show SIC where
  show (SIC d c) = diaget diaints d <> accstr augs '+' '-'
    where augs = c - dia2chrom d

instance AdditiveGroup SIC where
  zeroV = sic 0 0
  negateV (SIC d c) = sic (-d) (-c)
  (SIC d1 c1) ^+^ (SIC d2 c2) = sic (d1+d2) (c1+c2)
  (SIC d1 c1) ^-^ (SIC d2 c2) = sic (d1-d2) (c1-c2)

instance VectorSpace SIC where
  type Scalar SIC = Int
  s *^ (SIC d c) = sic (s*d) (s*c)

instance Interval SIC where
  type ICOf SIC = SIC
  ic = id
  toMidi (SIC _ c) = c
  octave n = zeroV
  direction (SIC 0 _) = EQ
  direction (SIC d _) = if d < 5 then GT else LT

instance Diatonic SIC where
  isStep (SIC d _) = d < 2 || d == 6

instance Chromatic SIC where
  chromaticSemitone = sic 0 1

instance IntervalClass SIC where
  type IOf SIC = SInterval
  -- ic (SInterval d c) = sic d c
  emb (SIC d c) = SInterval d c

instance EM.ToMusic1 (SIC) where
  toMusic1 = EM.toMusic1 . EM.mMap ((+60) . toMidi)

-------------
-- Pitches --
-------------

-- wrapper type: turn intervals into pitches
newtype Pitch a = Pitch a
  deriving (Eq, Ord, Generic)

instance NFData a => NFData (Pitch a)

instance Functor Pitch where
  fmap f (Pitch p) = Pitch (f p)

toPitch = Pitch
toInterval (Pitch i) = i

(Pitch a) `pto` (Pitch b) = b ^-^ a
(Pitch a) `pfrom` (Pitch b) = a ^-^ b
(Pitch p) +^ i = Pitch (p ^+^ i)
i ^+ (Pitch p) = Pitch (p ^+^ i)
(Pitch p) -^ i = Pitch (p ^-^ i)

-- this needs FlexibleInstances
-- instance Show (Pitch SInterval) where
--   show (Pitch i) = ""

-- instance Show (Pitch MidiInterval)

-- midi pitch
-------------

type MidiPitch = Pitch MidiInterval
type MidiPC    = Pitch MidiIC

instance Show (Pitch MidiInterval) where
  show (Pitch i) = "p" <> show i

instance Show (Pitch MidiIC) where
  show (Pitch i) = "pc" <> show i

-- spelled pitch / pitch class
------------------------------

type SPitch = Pitch SInterval
type SPC    = Pitch SIC

instance Show (Pitch SInterval) where
  show (Pitch (SInterval d c)) = diaget dianames d <> accstr accs '♯' '♭' <> show (div d 7)
    where accs = c - dia2chrom d

newtype Accidental = Acc Int
runAcc (Acc x) = x

flt = Acc (-1)
shp = Acc 1
nat = Acc 0

toSpelled dia chrom acc oct =
  Pitch $ SInterval (dia + 7*oct) (chrom + (runAcc acc) + 12*oct)

c = toSpelled 0 0
d = toSpelled 1 2
e = toSpelled 2 4
f = toSpelled 3 5
g = toSpelled 4 7
a = toSpelled 5 9
b = toSpelled 6 11

instance Show (Pitch SIC) where
  show (Pitch (SIC d c)) = diaget dianames d <> accstr accs '♯' '♭'
    where accs = c - dia2chrom d

toSPC dia chrom acc = Pitch $ sic dia (chrom + (runAcc acc))

c' = toSPC 0 0
d' = toSPC 1 2
e' = toSPC 2 4
f' = toSPC 3 5
g' = toSPC 4 7
a' = toSPC 5 9
b' = toSPC 6 11

----------------
-- Containers --
----------------

class (Num (TimeOf a), Ord (TimeOf a)) => Timed a where
  type TimeOf a

class Timed a => HasTime a where
  onset :: a -> TimeOf a
  onset x = offset x - duration x
  offset :: a -> TimeOf a
  offset x = onset x + duration x
  duration :: a -> TimeOf a
  duration x = offset x - onset x

class (Interval (IntervalOf a)) => Pitched a where
  type IntervalOf a

class Pitched a => HasInterval a where
  interval :: a -> IntervalOf a

class Pitched a => HasPitch a where
  pitch :: a -> Pitch (IntervalOf a)

-- some container helpers (put in separate module?)

embPs :: (Functor f, IntervalClass ic) => f (Pitch ic) -> f (Pitch (IOf ic))
embPs = fmap (fmap emb)

transposePs :: (Functor f, Interval i) => i -> f (Pitch i) -> f (Pitch i)
transposePs by = fmap (+^ by)

embed rot trans = transposePs trans . embPs . transposePs rot
embed' slide c0 = embed (negateV (ic slide)) (c0 ^+^ slide)

-- class (Pitch (PitchOf c), PitchContainer c) => AbsPitchContainer c where
--   refPitch :: c -> PitchOf c
--   transposeTo :: PitchOf c -> c -> c
--   transposeTo p c = transposeBy (p ^-^ refPitch c) c

-- some default instances
-------------------------

instance Interval p => Pitched [p] where
  type IntervalOf [p] = p

instance Interval p => Pitched (Maybe p) where
  type IntervalOf (Maybe p) = p
  
instance Interval p => Pitched (Identity p) where
  type IntervalOf (Identity p) = p

----------------
-- TimedEvent --
----------------

data TimedEvent c t = TimedEvent c t t
  deriving (Eq, Ord, Show, Read)

instance (Num t, Ord t) => Timed (TimedEvent p t) where
  type TimeOf (TimedEvent p t) = t

instance (Num t, Ord t) => HasTime (TimedEvent p t) where
  onset (TimedEvent _ on _) = on
  offset (TimedEvent _ _ off) = off

instance Pitched c => Pitched (TimedEvent c t) where
  type IntervalOf (TimedEvent c t) = IntervalOf c

-- instance Pitch p => Pitched (TimedEvent p t) where
--   pitch (TimedEvent p _ _) = p

-- instance PitchContainer c => PitchContainer (TimedEvent c t) where
--   mapPitches f (TimedEvent c on off) = TimedEvent (mapPitches f c) on off
--   pitches (TimedEvent c _ _) = pitches c

----------
-- Note --
----------

data Note p t = Note !(Pitch p) !t !t
  deriving (Eq, Ord, Generic)

instance (NFData p, NFData t) => NFData (Note p t) 

deriving instance (Show (Pitch p), Show t) => Show (Note p t)
deriving instance (Read (Pitch p), Read t) => Read (Note p t)

instance (Num t, Ord t) => Timed (Note p t) where
  type TimeOf (Note p t) = t

instance (Num t, Ord t) => HasTime (Note p t) where
  onset (Note _ on _) = on
  offset (Note _ _ off) = off

instance Interval p => Pitched (Note p t) where
  type IntervalOf (Note p t) = p

instance Interval p => HasPitch (Note p t) where
  pitch (Note p _ _) = p

instance (ToJSON p, ToJSON t) => ToJSON (Note p t) where
  toJSON (Note (Pitch p) on off) =
    object ["pitch" .= p, "onset" .= on, "offset" .= off]
  toEncoding (Note (Pitch p) on off) =
    pairs ("pitch" .= p <> "onset" .= on <> "offset" .= off)

----------------------------------------------
-- Ons and Offs                             --
-- - make onset and offset separate objects --
----------------------------------------------

data OnOff c t = Onset c !t
               | Offset c !t
  deriving (Eq, Ord, Show, Read, Generic)

isOn (Onset _ _) = True
isOn (Offset _ _) = False
ifOff (Onset _ _) = False
isOff (Offset _ _) = True

instance (Num t, Ord t) => Timed (OnOff c t) where
  type TimeOf (OnOff c t) = t

instance (Num t, Ord t) => HasTime (OnOff p t) where
  onset (Onset _ t) = t
  onset (Offset _ t) = t
  offset (Onset _ t) = t
  offset (Offset _ t) = t

instance Pitched c => Pitched (OnOff c t) where
  type IntervalOf (OnOff c t) = IntervalOf c

instance HasInterval c => HasInterval (OnOff c t) where
  interval (Onset c _) = interval c
  interval (Offset c _) = interval c

instance HasPitch c => HasPitch (OnOff c t) where
  pitch (Onset c _) = pitch c
  pitch (Offset c _) = pitch c

---------------------------------
-- contextual note: held over? --
---------------------------------
