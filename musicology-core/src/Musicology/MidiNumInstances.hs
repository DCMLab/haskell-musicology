{-# LANGUAGE FlexibleInstances #-}
module Musicology.MidiNumInstances where

import Musicology.Types

instance Num (Pitch MidiInterval) where
  fromInteger = Pitch . fromInteger

instance Num (Pitch MidiIC) where
  fromInteger = Pitch . mic . fromInteger

instance Num MidiIC where
  fromInteger = mic . fromInteger
  (MidiIC a) + (MidiIC b) = mic $ a+b
  (MidiIC a) - (MidiIC b) = mic $ a-b
  (MidiIC a) * (MidiIC b) = mic $ a*b -- never use this one!
  negate (MidiIC i) = mic (-i)
  abs = id
  signum (MidiIC i) = mic $ signum i

