module Musicology.Euterpea where

import qualified Euterpea.Music as EM
import Musicology.Pitch

instance EM.ToMusic1 MidiIC where
  toMusic1 = EM.toMusic1 . EM.mMap (\i -> (emb i + 60))

instance EM.ToMusic1 (SInterval) where
  toMusic1 = EM.toMusic1 . EM.mMap toMidi

instance EM.ToMusic1 (SIC) where
  toMusic1 = EM.toMusic1 . EM.mMap ((+60) . toMidi)

instance EM.ToMusic1 p => EM.ToMusic1 (Pitch p) where
  toMusic1 = EM.toMusic1 . EM.mMap toInterval

