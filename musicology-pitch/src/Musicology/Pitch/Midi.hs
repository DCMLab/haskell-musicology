{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module: Musicology.Pitch.Midi
Description: Enharmonic pitch and interval types (MIDI).
Copyright: Christoph Finkensiep, 2021
License: BSD
Maintainer: chfin@chfin.de
Stability: experimental

This module defines pitch and interval types for enharmonic/chromatic pitch, (as used in MIDI).
Midi intervals are just 'Int's.
-}
module Musicology.Pitch.Midi
  ( -- * Interval types
    MidiInterval
  , MidiIC(..)
  , mic
  -- * Pitch types
  , MidiPitch
  , MidiPC
  , midip
  , midipc
  ) where

import           Musicology.Pitch.Class
import           Musicology.Pitch.Internal

import           Control.DeepSeq                ( NFData )
import           Data.Hashable                  ( Hashable )
import qualified Text.ParserCombinators.ReadP  as R
import           Text.Read                      ( readMaybe )

-- MidiInterval
---------------

newtype MidiIC = MidiIC Int
  deriving (Eq, Ord, NFData, Show, Hashable)

mic :: Int -> MidiIC
mic = MidiIC . flip mod 12

instance AdditiveGroup MidiIC where
  zeroV = MidiIC 0
  negateV (MidiIC m) = mic $ negate m
  (MidiIC a) ^+^ (MidiIC b) = mic $ a + b
  (MidiIC a) ^-^ (MidiIC b) = mic $ a - b

instance VectorSpace MidiIC where
  type Scalar MidiIC = Int
  s *^ (MidiIC i) = mic (s * i)

instance Interval MidiIC where
  type ICOf MidiIC = MidiIC
  ic     = id
  octave = mic 0
  direction (MidiIC 0) = EQ
  direction (MidiIC i) = if i == 0 then EQ else compare 6 i

instance IntervalClass MidiIC where
  type IOf MidiIC = MidiInterval
  -- ic = mic
  emb (MidiIC i) = i

instance Diatonic MidiIC where
  isStep (MidiIC i) = i <= 2 || i < 12 && i > 9 -- no i < 12?

instance Chromatic MidiIC where
  chromaticSemitone = MidiIC 1

instance Notation MidiIC where
  showNotation (MidiIC i) = "ic" <> show i
  parseNotation = R.string "ic" >> mic <$> parseInt

instance ToMidi MidiIC where
  toMidi (MidiIC x) = x

type MidiInterval = Int
--  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance Interval Int where
  type ICOf Int = MidiIC
  ic     = mic
  octave = 12
--  direction i = compare i 0
--  icInt int = if int > 6 then int - 12 else int

-- instance ClassyInterval Int where
--   type PCOf Int = MidiIC

instance Diatonic Int where
  isStep a = abs a <= 2

instance Chromatic Int where
  chromaticSemitone = 1

instance Notation MidiInterval where
  showNotation  = show
  parseNotation = parseInt
  readNotation  = readMaybe

instance ToMidi MidiInterval where
  toMidi = id

-- midi pitch
-------------

type MidiPitch = Pitch MidiInterval
type MidiPC = Pitch MidiIC

midip :: Int -> MidiPitch
midip = Pitch

midipc :: Int -> MidiPC
midipc = Pitch . mic

instance Notation MidiPitch where
  showNotation (Pitch i) = "p" <> showNotation i
  parseNotation = R.char 'p' >> (midip <$> parseInt)

instance Show MidiPitch where
  show = showNotation

instance ToMidi MidiPitch where
  toMidi (Pitch i) = i

instance Notation MidiPC where
  showNotation (Pitch (MidiIC i)) = "pc" <> showNotation i
  parseNotation = R.string "pc" >> (midipc <$> parseInt)

instance Show MidiPC where
  show = showNotation

instance ToMidi MidiPC where
  toMidi (Pitch (MidiIC ic)) = 60 + ic
