{-# LANGUAGE FlexibleContexts, DataKinds, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Musicology.IO.CSV where

import Frames
import Frames.TH
import Lens.Micro
import Lens.Micro.Extras
import qualified Data.Foldable as F

import Musicology.Types as Mus hiding (onset, offset, pitch)

fn = "/home/chfin/Uni/phd/data/midi_archive/notes/s/k555.tsv"

tableTypes' (rowGen "/home/chfin/Uni/phd/data/midi_archive/notes/s/k555.tsv")
  { rowTypeName = "CsvNote"
--  , columnNames = ["Track", "Channel", "Onset", "Offset", "Pitch", "Velocity"]
  , separator = "\t"
  , tablePrefix = "csv"
  }

loadPiece :: FilePath -> IO (Frame CsvNote)
loadPiece fp = inCoreAoS $ readTableOpt csvNoteParser fp

frameNotes :: Frame CsvNote -> [Note MidiPitch Int]
frameNotes fr = fmap toNote $ F.toList fr
  where toNote row = Note (view csvPitch row) (view csvOnset row) (view csvOffset row)
