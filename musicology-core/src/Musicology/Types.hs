-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Musicology.Types
  ( PitchClass(..)
  , Pitch(..)
  -- , ClassyPitch(..)
  , Diatonic(..)
  , VectorSpace(..), AdditiveGroup(..)
  , MidiPitch, MidiPC(..), mpc
  , SPitch(..), SPC(..), spc
  , flt, shp, nat, c, d, e, f, g, a, b
  , aug, dim, down
  , unison, minsec, majsec, minthd, majthd, fourth
  , tritone, fifth, minsix, majsix, minsev, majsev
  , c', d', e', f', g', a', b'
  , unison', minsec', majsec', minthd', majthd', fourth'
  , tritone', fifth', minsix', majsix', minsev', majsev'
  , HasTime(..), Timed(..)
  , HasPitch(..), Pitched(..)
  , PitchClassContainer(..), PitchContainer(..), embed, embed'
  , AbsPitchContainer(..)
  , TimedEvent(..)
  , Note(..)
  , OnOff(..), isOn, isOff
  ) where

import Data.Aeson
import Data.Monoid ((<>))

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Data.VectorSpace

import qualified Euterpea.Music as EM

-------------
-- Pitches --
-------------

-- should pitches always be nums?
-- does multiplication always make sense?
-- scalar multiplication? vector space?

class (Pitch p, Pitch (POf p), VectorSpace p, PCOf (POf p) ~ p) => PitchClass p where
  type POf p
--  pc :: POf p -> p
  emb :: p -> POf p

class VectorSpace p => Pitch p where
  type PCOf p
  pc :: p -> PCOf p
  toMidi :: p -> MidiPitch
  toFreq :: p -> Double
  toFreq p = 440 * 2 ** ((fromIntegral (toMidi p) - 9) / 12)
  octave :: Int -> p
  direction :: p -> Ordering
  default direction :: Ord p => p -> Ordering
  direction p = compare p zeroV

-- class (Pitch p, PitchClass (PCOf p), POf (PCOf p) ~ p) => ClassyPitch p where  

class Pitch p => Diatonic p where
  isStep :: p -> Bool

-- MidiPitch
------------

newtype MidiPC = MidiPC Int
  deriving (Eq, Ord, NFData)

mpc :: Int -> MidiPC
mpc = MidiPC . flip mod 12

instance Show MidiPC where
  show (MidiPC p) = show p

instance Num MidiPC where
  fromInteger = mpc . fromInteger
  (MidiPC a) + (MidiPC b) = mpc $ a+b
  (MidiPC a) - (MidiPC b) = mpc $ a-b
  (MidiPC a) * (MidiPC b) = mpc $ a*b -- never use this one!
  negate (MidiPC p) = mpc (-p)
  abs = id
  signum (MidiPC p) = mpc $ signum p

instance AdditiveGroup MidiPC where
  zeroV = MidiPC 0
  negateV = negate
  (^+^) = (+)
  (^-^) = (-)

instance VectorSpace MidiPC where
  type Scalar MidiPC = Int
  s *^ (MidiPC p) = mpc (s*p)

instance Pitch MidiPC where
  type PCOf MidiPC = MidiPC
  pc = id
  toMidi (MidiPC p) = p+60
  octave x = 0
  direction (MidiPC 0) = EQ
  direction (MidiPC p) = if p == 0
                         then EQ
                         else compare 6 p

instance PitchClass MidiPC where
  type POf MidiPC = MidiPitch
  -- pc = mpc
  emb (MidiPC p) = p

instance Diatonic MidiPC where
  isStep (MidiPC p) = p <= 2 || p < 12 && p > 9 -- no p < 12?

instance EM.ToMusic1 (MidiPC) where
  toMusic1 = EM.toMusic1 . EM.mMap (\p -> (emb p + 60))

type MidiPitch = Int
--  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance Pitch Int where
  type PCOf Int = MidiPC
  pc = mpc
  toMidi = id
  toFreq p = 440 * 2 ** ((fromIntegral p - 69)/12)
  octave = (12*)
--  direction p = compare p 0
--  pcInt int = if int > 6 then int - 12 else int

-- instance ClassyPitch Int where
--   type PCOf Int = MidiPC

instance Diatonic Int where
  isStep a = (abs a) <= 2

-- Spelled Pitch
----------------

-- data NoteNames = A | B | C | D | E | F | G
--   deriving (Eq, Ord, Show)

-- spelled pitch type and instances

diachrom = [0, 2, 4, 5, 7, 9, 11]
dia2chrom d = (diachrom !! (mod d 7)) + 12 * (div d 7)
dianames = ["c", "d", "e", "f", "g", "a", "b"]
dianame = (dianames!!) . (flip mod 7)
accstr naccs = if naccs == 0
               then ""
               else if naccs > 0
                    then take naccs $ repeat '#'
                    else take (abs naccs) $ repeat 'b'

data SPitch = SPitch
                    { dSteps :: Int
                    , cSteps :: Int
                    }
  deriving (Ord, Eq, Generic)

flt = (-1) :: Int
shp = 1 :: Int
nat = 0 :: Int

toSpelled dia chrom oct acc = SPitch (dia + 7*oct) (chrom + acc + 12*oct)

c = toSpelled 0 0
d = toSpelled 1 2
e = toSpelled 2 4
f = toSpelled 3 5
g = toSpelled 4 7
a = toSpelled 5 9
b = toSpelled 6 11

dim (SPitch d c) = (SPitch d (c-1))
aug (SPitch d c) = (SPitch d (c+1))
down x = negateV x

unison = SPitch 0 0
minsec = SPitch 1 1
majsec = SPitch 1 2
minthd = SPitch 2 3
majthd = SPitch 2 4
fourth = SPitch 3 5
tritone = SPitch 3 6
fifth = SPitch 4 7
minsix = SPitch 5 8
majsix = SPitch 5 9
minsev = SPitch 6 10
majsev = SPitch 6 11

instance NFData SPitch

instance Show SPitch where
  show (SPitch d c) = dianame d <> accstr accs <> show (div d 7)
    where accs = c - dia2chrom d

instance AdditiveGroup SPitch where
  zeroV = SPitch 0 0
  negateV (SPitch d c) = SPitch (-d) (-c)
  (SPitch d1 c1) ^+^ (SPitch d2 c2) = SPitch (d1+d2) (c1+c2)
  (SPitch d1 c1) ^-^ (SPitch d2 c2) = SPitch (d1-d2) (c1-c2)

instance VectorSpace SPitch where
  type Scalar SPitch = Int
  s *^ (SPitch d c) = SPitch (s*d) (s*c)

instance Pitch SPitch where
  type PCOf SPitch = SPC
  pc (SPitch d c) = spc d c
  toMidi (SPitch d c) = c + 12
  octave n = SPitch (7*n) (12*n)
--  direction (SPitch d _) = compare d 0 -- is this a good idea?

-- instance ClassyPitch SPitch where
--   type PCOf SPitch = SPC

instance Diatonic SPitch where
  isStep (SPitch d _) = abs d < 2

instance EM.ToMusic1 (SPitch) where
  toMusic1 = EM.toMusic1 . EM.mMap toMidi

-- spelled pitch class type and instances

data SPC = SPC Int Int
  deriving (Ord, Eq, Generic)

spc d c = SPC (mod d 7) (mod c 12)

toSpc dia chrom acc = spc dia (chrom + acc)

c' = toSpc 0 0
d' = toSpc 1 2
e' = toSpc 2 4
f' = toSpc 3 5
g' = toSpc 4 7
a' = toSpc 5 9
b' = toSpc 6 11

unison' = spc 0 0
minsec' = spc 1 1
majsec' = spc 1 2
minthd' = spc 2 3
majthd' = spc 2 4
fourth' = spc 3 5
tritone' = spc 3 6
fifth' = spc 4 7
minsix' = spc 5 8
majsix' = spc 5 9
minsev' = spc 6 10
majsev' = spc 6 11

instance NFData SPC

instance Show SPC where
  show (SPC d c) = dianame d <> accstr accs
    where accs = c - dia2chrom d

instance AdditiveGroup SPC where
  zeroV = spc 0 0
  negateV (SPC d c) = spc (-d) (-c)
  (SPC d1 c1) ^+^ (SPC d2 c2) = spc (d1+d2) (c1+c2)
  (SPC d1 c1) ^-^ (SPC d2 c2) = spc (d1-d2) (c1-c2)

instance VectorSpace SPC where
  type Scalar SPC = Int
  s *^ (SPC d c) = spc (s*d) (s*c)

instance Pitch SPC where
  type PCOf SPC = SPC
  pc = id
  toMidi (SPC _ c) = c
  octave n = zeroV
  direction (SPC 0 _) = EQ
  direction (SPC d _) = if d < 5 then GT else LT

instance Diatonic SPC where
  isStep (SPC d _) = d < 2 || d == 6

instance PitchClass SPC where
  type POf SPC = SPitch
  -- pc (SPitch d c) = spc d c
  emb (SPC d c) = SPitch d c

instance EM.ToMusic1 (SPC) where
  toMusic1 = EM.toMusic1 . EM.mMap ((+60) . toMidi)

----------------
-- Containers --
----------------

class (Num (TimeOf a), Ord (TimeOf a)) => HasTime a where
  type TimeOf a

class HasTime a => Timed a where
  onset :: a -> TimeOf a
  onset x = offset x - duration x
  offset :: a -> TimeOf a
  offset x = onset x + duration x
  duration :: a -> TimeOf a
  duration x = offset x - onset x

class (Pitch (PitchOf a)) => HasPitch a where
  type PitchOf a

class HasPitch a => Pitched a where
  pitch :: a -> PitchOf a

class (PitchContainer c, PitchContainer (EmbCont c),
       PitchClass (PitchOf c), PitchOf (EmbCont c) ~ POf (PitchOf c)) =>
      PitchClassContainer c where
  type EmbCont c
  embPitches :: c -> EmbCont c

class HasPitch c => PitchContainer c where
  mapPitches :: (PitchOf c -> PitchOf c) -> c -> c
  pitches :: c -> [PitchOf c]
  transposeBy :: PitchOf c -> c -> c
  transposeBy int = mapPitches (^+^int)

-- embed :: (PitchContainer g, PitchClass (PitchOf g), PitchOf gp ~ POf (PitchOf g)) =>
--   PitchOf g -> POf (PitchOf g) -> g -> gp
-- embed rot trans = transposeBy trans . mapPitches emb . transposeBy rot

-- embed :: (PitchClass p, PitchContainer (c p), Functor c, PitchContainer (c (POf p)),
--           PitchOf (c (POf p)) ~ POf p, PitchOf (c p) ~ p) =>
--          p -> POf p -> c p -> c (POf p)  
embed :: (PitchClassContainer c, PCOf (POf (PitchOf c)) ~ PitchOf c) =>
         PitchOf c -> POf (PitchOf c) -> c -> EmbCont c
embed rot trans = transposeBy trans . embPitches . transposeBy rot

-- embed' :: _ => POf p -> POf p -> c p -> c (POf p)
embed' :: (PitchClassContainer c, PCOf (POf (PitchOf c)) ~ PitchOf c) =>
          POf (PitchOf c) -> POf (PitchOf c) -> c -> EmbCont c
embed' slide c0 = embed (negateV (pc slide)) (c0 ^+^ slide)

class (Pitch (PitchOf c), PitchContainer c) => AbsPitchContainer c where
  refPitch :: c -> PitchOf c
  transposeTo :: PitchOf c -> c -> c
  transposeTo p c = transposeBy (p ^-^ refPitch c) c

-- some default instances
-------------------------

instance Pitch p => HasPitch [p] where
  type PitchOf [p] = p

instance PitchClass p => PitchClassContainer [p] where
  type EmbCont [p] = [POf p]
  embPitches = map emb

instance Pitch p => PitchContainer [p] where
  mapPitches = map
  pitches ps = ps

instance Pitch p => AbsPitchContainer [p] where
  refPitch = head

-- instance (Pitch p, PitchContainer c p) => PitchContainer [c] p where
--   pitches pss = pss >>= pitches
--   transposeBy p = map (transposeBy p)

----------------
-- TimedEvent --
----------------

data TimedEvent c t = TimedEvent c t t
  deriving (Eq, Ord, Show, Read)

instance (Num t, Ord t) => HasTime (TimedEvent p t) where
  type TimeOf (TimedEvent p t) = t

instance (Num t, Ord t) => Timed (TimedEvent p t) where
  onset (TimedEvent _ on _) = on
  offset (TimedEvent _ _ off) = off

instance HasPitch c => HasPitch (TimedEvent c t) where
  type PitchOf (TimedEvent c t) = PitchOf c

-- instance Pitch p => Pitched (TimedEvent p t) where
--   pitch (TimedEvent p _ _) = p

instance PitchContainer c => PitchContainer (TimedEvent c t) where
  mapPitches f (TimedEvent c on off) = TimedEvent (mapPitches f c) on off
  pitches (TimedEvent c _ _) = pitches c

----------
-- Note --
----------

data Note p t = Note !p !t !t
  deriving (Eq, Ord, Show, Read, Generic)

instance (NFData p, NFData t) => NFData (Note p t) 

instance (Num t, Ord t) => HasTime (Note p t) where
  type TimeOf (Note p t) = t

instance (Num t, Ord t) => Timed (Note p t) where
  onset (Note _ on _) = on
  offset (Note _ _ off) = off

instance Pitch p => HasPitch (Note p t) where
  type PitchOf (Note p t) = p

instance Pitch p => Pitched (Note p t) where
  pitch (Note p _ _) = p

instance (ToJSON p, ToJSON t) => ToJSON (Note p t) where
  toJSON (Note p on off) =
    object ["pitch" .= p, "onset" .= on, "offset" .= off]
  toEncoding (Note p on off) =
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

instance (Num t, Ord t) => HasTime (OnOff c t) where
  type TimeOf (OnOff c t) = t

instance (Num t, Ord t) => Timed (OnOff p t) where
  onset (Onset _ t) = t
  onset (Offset _ t) = t
  offset (Onset _ t) = t
  offset (Offset _ t) = t

instance HasPitch c => HasPitch (OnOff c t) where
  type PitchOf (OnOff c t) = PitchOf c

instance Pitched c => Pitched (OnOff c t) where
  pitch (Onset c _) = pitch c
  pitch (Offset c _) = pitch c

---------------------------------
-- contextual note: held over? --
---------------------------------
