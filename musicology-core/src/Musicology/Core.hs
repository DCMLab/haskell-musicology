{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Musicology.Core
  ( module Musicology.Pitch
  , Timed(..), HasTime(..)
  , Pitched(..), HasInterval(..), HasPitch(..)
  , Identifiable(..)
  , TimedEvent(..), timedEventContent
  , Note(..), NoteId(..)
  , OnOff(..), isOn, isOff
  ) where

import Musicology.Pitch

import Data.Functor.Identity (Identity(..))

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Lens.Micro
import Lens.Micro.Extras

import Data.Aeson

----------------
-- Containers --
----------------

class (Num (TimeOf a), Ord (TimeOf a)) => Timed a where
  type TimeOf a

class Timed a => HasTime a where
  onsetL :: Lens' a (TimeOf a)
  onsetL = lens onset (flip setOnset)
  offsetL :: Lens' a (TimeOf a)
  offsetL = lens offset (flip setOffset)
  onset :: a -> TimeOf a
  onset = view onsetL
  offset :: a -> TimeOf a
  offset = view offsetL
  setOnset :: TimeOf a -> a -> a
  setOnset = set onsetL
  setOffset :: TimeOf a -> a -> a
  setOffset = set offsetL

class (Interval (IntervalOf a)) => Pitched a where
  type IntervalOf a

class Pitched a => HasInterval a where
  intervalL :: Lens' a (IntervalOf a)
  intervalL = lens interval (flip setInterval)
  interval :: a -> IntervalOf a
  interval = view intervalL
  setInterval :: IntervalOf a -> a -> a
  setInterval = set intervalL

class Pitched a => HasPitch a where
  pitchL :: Lens' a (Pitch (IntervalOf a))
  pitchL = lens pitch (flip setPitch)
  pitch :: a -> Pitch (IntervalOf a)
  pitch = view pitchL
  setPitch :: Pitch (IntervalOf a) -> a -> a
  setPitch = set pitchL

-- some container helpers (put in separate module?)

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

---------
-- IDs --
---------

class Identifiable i where
  type IdOf i
  getId :: i -> IdOf i

----------------
-- TimedEvent --
----------------

data TimedEvent c t = TimedEvent c t t
  deriving (Eq, Ord, Show, Read)

timedEventContent (TimedEvent c _ _) = c

instance (Num t, Ord t) => Timed (TimedEvent p t) where
  type TimeOf (TimedEvent p t) = t

instance (Num t, Ord t) => HasTime (TimedEvent p t) where
  onsetL f (TimedEvent e on off) = fmap (\on' -> TimedEvent e on' off) (f on)
  offsetL f (TimedEvent e on off) = fmap (\off' -> TimedEvent e on off') (f off)

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
  onsetL f (Note p on off) = fmap (\on' -> Note p on' off) (f on)
  offsetL f (Note p on off) = fmap (Note p on) (f off)

instance Interval p => Pitched (Note p t) where
  type IntervalOf (Note p t) = p

instance Interval p => HasPitch (Note p t) where
  pitchL f (Note p on off) = fmap (\p' -> Note p' on off) (f p)

instance (ToJSON p, ToJSON t) => ToJSON (Note p t) where
  toJSON (Note (Pitch p) on off) =
    object ["pitch" .= p, "onset" .= on, "offset" .= off]
  toEncoding (Note (Pitch p) on off) =
    pairs ("pitch" .= p <> "onset" .= on <> "offset" .= off)

instance (FromJSON p, FromJSON t) => FromJSON (Note p t) where
  parseJSON = withObject "Note" $ \v -> Note
    <$> (toPitch <$> v .: "pitch")
    <*> v .: "onset"
    <*> v .: "offset"

-- note with id
---------------

data NoteId p t i = NoteId !(Pitch p) !t !t !i
  deriving (Eq, Ord, Generic)

instance (NFData p, NFData t, NFData i) => NFData (NoteId p t i)

deriving instance (Show (Pitch p), Show t, Show i) => Show (NoteId p t i)
deriving instance (Read (Pitch p), Read t, Read i) => Read (NoteId p t i)

instance (Num t, Ord t) => Timed (NoteId p t i) where
  type TimeOf (NoteId p t i) = t

instance (Num t, Ord t) => HasTime (NoteId p t i) where
  onsetL f (NoteId p on off id) = fmap (\on' -> NoteId p on' off id) (f on)
  offsetL f (NoteId p on off id) = fmap (\off' -> NoteId p on off' id) (f off)

instance Interval p => Pitched (NoteId p t i) where
  type IntervalOf (NoteId p t i) = p

instance Interval p => HasPitch (NoteId p t i) where
  pitchL f (NoteId p on off id) = fmap (\p' -> NoteId p' on off id) (f p)

instance Identifiable (NoteId p t i) where
  type IdOf (NoteId p t i) = i
  getId (NoteId _ _ _ i) = i

instance (ToJSON p, ToJSON t, ToJSON i) => ToJSON (NoteId p t i) where
  toJSON (NoteId (Pitch p) on off id) =
    object ["pitch" .= p, "onset" .= on, "offset" .= off, "id" .= id]
  toEncoding (NoteId (Pitch p) on off id) =
    pairs ("pitch" .= p <> "onset" .= on <> "offset" .= off <> "id" .= id)

instance (FromJSON p, FromJSON t, FromJSON i) => FromJSON (NoteId p t i) where
  parseJSON = withObject "Note" $ \v -> NoteId
    <$> (toPitch <$> v .: "pitch")
    <*> v .: "onset"
    <*> v .: "offset"
    <*> v .: "id"


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

onOffContent :: Lens' (OnOff c t) c
onOffContent f (Onset c t) = fmap (`Onset` t) (f c)
onOffContent f (Offset c t) = fmap (`Offset` t) (f c)

instance (Num t, Ord t) => Timed (OnOff c t) where
  type TimeOf (OnOff c t) = t

instance (Num t, Ord t) => HasTime (OnOff p t) where
  onsetL f (Onset p t) = fmap (Onset p) (f t)
  onsetL f (Offset p t) = fmap (Offset p) (f t)
  offsetL = onsetL

instance Pitched c => Pitched (OnOff c t) where
  type IntervalOf (OnOff c t) = IntervalOf c

instance HasInterval c => HasInterval (OnOff c t) where
  intervalL = onOffContent . intervalL

instance HasPitch c => HasPitch (OnOff c t) where
  pitchL = onOffContent . pitchL

---------------------------------
-- contextual note: held over? --
---------------------------------
