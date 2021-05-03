{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Musicology.Core
  ( module Musicology.Pitch
  , Timed(..)
  , HasTime(..)
  , Pitched(..)
  , HasInterval(..)
  , HasPitch(..)
  , Identifiable(..)
  , TimedEvent(..)
  , timedEventContent
  , Note(..)
  , NoteId(..)
  , OnOff(..)
  , onOffContent
  , isOn
  , isOff
  , Tied(..)
  , LeftTied(..)
  , RightTied(..)
  , rightTie
  , leftTie
  , fullTie
  )
where

import           Musicology.Pitch

import           Data.Functor.Identity          ( Identity(..) )

import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )
import           Data.Hashable                  ( Hashable )

import           Lens.Micro
import           Lens.Micro.Extras

import           Data.Aeson

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

class (Interval (IntervalOf a),
       ReTypeInterval a (IntervalOf a) ~ a) => Pitched a where
  type IntervalOf a
  type ReTypeInterval a p

class Pitched a => HasInterval a where
  intervalL :: (IntervalOf (ReTypeInterval a p2) ~ p2)
            => Lens a (ReTypeInterval a p2) (IntervalOf a) p2
  intervalL = lens interval (flip setInterval)
  interval :: a -> IntervalOf a
  interval = view intervalL
  setInterval :: (IntervalOf (ReTypeInterval a p2) ~ p2)
              => p2 -> a -> ReTypeInterval a p2
  setInterval = set intervalL

class (Pitched a) => HasPitch a where
  pitchL :: (IntervalOf (ReTypeInterval a p2) ~ p2)
         => Lens a (ReTypeInterval a p2) (Pitch (IntervalOf a)) (Pitch p2)
  pitchL = lens pitch (flip setPitch)
  pitch :: a -> Pitch (IntervalOf a)
  pitch = view pitchL
  setPitch :: (IntervalOf (ReTypeInterval a p2) ~ p2)
           => Pitch p2 -> a -> ReTypeInterval a p2
  setPitch = set pitchL

-- some container helpers (put in separate module?)

-- class (Pitch (PitchOf c), PitchContainer c) => AbsPitchContainer c where
--   refPitch :: c -> PitchOf c
--   transposeTo :: PitchOf c -> c -> c
--   transposeTo p c = transposeBy (p ^-^ refPitch c) c

-- some default instances
-------------------------

instance Interval i => Pitched (Pitch i) where
  type IntervalOf (Pitch i) = i
  type ReTypeInterval (Pitch i) i' = Pitch i'

instance Interval i => HasPitch (Pitch i) where
  pitch = id
  setPitch = const

instance Interval p => Pitched [p] where
  type IntervalOf [p] = p
  type ReTypeInterval [p] p2 = [p2]

instance Interval p => Pitched (Maybe p) where
  type IntervalOf (Maybe p) = p
  type ReTypeInterval (Maybe p) p2 = Maybe p2

instance Interval p => Pitched (Identity p) where
  type IntervalOf (Identity p) = p
  type ReTypeInterval (Identity p) p2 = Identity p2

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
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

timedEventContent (TimedEvent c _ _) = c

instance (Num t, Ord t) => Timed (TimedEvent p t) where
  type TimeOf (TimedEvent p t) = t

instance (Num t, Ord t) => HasTime (TimedEvent p t) where
  onsetL f (TimedEvent e on off) = fmap (\on' -> TimedEvent e on' off) (f on)
  offsetL f (TimedEvent e on off) = fmap (TimedEvent e on) (f off)

instance Pitched c => Pitched (TimedEvent c t) where
  type IntervalOf (TimedEvent c t) = IntervalOf c
  type ReTypeInterval (TimedEvent c t) p2 = TimedEvent (ReTypeInterval c p2) t

-- instance Pitch p => Pitched (TimedEvent p t) where
--   pitch (TimedEvent p _ _) = p

-- instance PitchContainer c => PitchContainer (TimedEvent c t) where
--   mapPitches f (TimedEvent c on off) = TimedEvent (mapPitches f c) on off
--   pitches (TimedEvent c _ _) = pitches c

----------
-- Note --
----------

data Note p t = Note !(Pitch p) !t !t
  deriving (Eq, Ord, Generic, NFData, Hashable)

deriving instance (Show (Pitch p), Show t) => Show (Note p t)
deriving instance (Read (Pitch p), Read t) => Read (Note p t)

instance (Num t, Ord t) => Timed (Note p t) where
  type TimeOf (Note p t) = t

instance (Num t, Ord t) => HasTime (Note p t) where
  onsetL f (Note p on off) = fmap (\on' -> Note p on' off) (f on)
  offsetL f (Note p on off) = fmap (Note p on) (f off)

instance Interval p => Pitched (Note p t) where
  type IntervalOf (Note p t) = p
  type ReTypeInterval (Note p t) p2 = Note p2 t

instance Interval p => HasPitch (Note p t) where
  pitchL :: Lens (Note p t) (Note p2 t) (Pitch p) (Pitch p2)
  pitchL f (Note p on off) = fmap (\p' -> Note p' on off) (f p)

instance (ToJSON p, ToJSON t) => ToJSON (Note p t) where
  toJSON (Note (Pitch p) on off) =
    object ["pitch" .= p, "onset" .= on, "offset" .= off]
  toEncoding (Note (Pitch p) on off) =
    pairs ("pitch" .= p <> "onset" .= on <> "offset" .= off)

instance (FromJSON p, FromJSON t) => FromJSON (Note p t) where
  parseJSON = withObject "Note" $ \v ->
    Note <$> (toPitch <$> v .: "pitch") <*> v .: "onset" <*> v .: "offset"

-- note with id
---------------

data NoteId p t i = NoteId !(Pitch p) !t !t !i
  deriving (Eq, Ord, Generic, NFData, Hashable)

deriving instance (Show (Pitch p), Show t, Show i) => Show (NoteId p t i)
deriving instance (Read (Pitch p), Read t, Read i) => Read (NoteId p t i)

instance (Num t, Ord t) => Timed (NoteId p t i) where
  type TimeOf (NoteId p t i) = t

instance (Num t, Ord t) => HasTime (NoteId p t i) where
  onsetL f (NoteId p on off id) = fmap (\on' -> NoteId p on' off id) (f on)
  offsetL f (NoteId p on off id) = fmap (\off' -> NoteId p on off' id) (f off)

instance Interval p => Pitched (NoteId p t i) where
  type IntervalOf (NoteId p t i) = p
  type ReTypeInterval (NoteId p t i) p2 = NoteId p2 t i

instance Interval p => HasPitch (NoteId p t i) where
  pitchL :: Lens (NoteId p t i) (NoteId p2 t i) (Pitch p) (Pitch p2)
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
  parseJSON = withObject "Note" $ \v ->
    NoteId
      <$> (toPitch <$> v .: "pitch")
      <*> v
      .:  "onset"
      <*> v
      .:  "offset"
      <*> v
      .:  "id"


----------------------------------------------
-- Ons and Offs                             --
-- - make onset and offset separate objects --
----------------------------------------------

data OnOff c t = Onset c !t
               | Offset c !t
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable)

isOn (Onset  _ _) = True
isOn (Offset _ _) = False
ifOff (Onset _ _) = False
isOff (Offset _ _) = True

onOffContent :: Lens (OnOff c t) (OnOff c2 t) c c2
onOffContent f (Onset  c t) = fmap (`Onset` t) (f c)
onOffContent f (Offset c t) = fmap (`Offset` t) (f c)

instance (Num t, Ord t) => Timed (OnOff c t) where
  type TimeOf (OnOff c t) = t

instance (Num t, Ord t) => HasTime (OnOff p t) where
  onsetL f (Onset  p t) = fmap (Onset p) (f t)
  onsetL f (Offset p t) = fmap (Offset p) (f t)
  offsetL = onsetL

instance Pitched c => Pitched (OnOff c t) where
  type IntervalOf (OnOff c t) = IntervalOf c
  type ReTypeInterval (OnOff c t) p2 = OnOff (ReTypeInterval c p2) t

instance HasInterval c => HasInterval (OnOff c t) where
  intervalL = onOffContent . intervalL

instance HasPitch c => HasPitch (OnOff c t) where
  pitchL = onOffContent . pitchL

----------------------------------------
-- tied events: continued/continuing? --
----------------------------------------

data Tied = Single
          | Starts
          | Continues
          | Stops
  deriving (Show, Eq, Ord, Generic, NFData, Hashable)

data RightTied = Holds
               | Ends
  deriving (Show, Eq, Ord, Generic, NFData, Hashable)

rightTie :: Tied -> RightTied
rightTie Single    = Ends
rightTie Starts    = Holds
rightTie Continues = Holds
rightTie Stops     = Ends

data LeftTied = New
              | Held
  deriving (Show, Eq, Ord, Generic, NFData, Hashable)

leftTie :: Tied -> LeftTied
leftTie Single    = New
leftTie Starts    = New
leftTie Continues = Held
leftTie Stops     = Held

fullTie :: LeftTied -> RightTied -> Tied
fullTie New  Ends  = Single
fullTie New  Holds = Starts
fullTie Held Holds = Continues
fullTie Held Ends  = Stops
