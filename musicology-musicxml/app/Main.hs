{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Musicology.MusicXML
import Musicology.Pitch (SPitch, showNotationT)
import qualified Pipes as P
import Frames.CSV (produceCSV)
import Frames.ShowCSV
import Data.Ratio
import qualified Data.Text as T

instance ShowCSV i => ShowCSV (Ratio i) where
  showCSV r = showCSV (numerator r) <> "//" <> showCSV (denominator r)

instance ShowCSV String where
  showCSV = showCSV . T.pack

instance ShowCSV a => ShowCSV (Maybe a) where
  showCSV Nothing  = "NA"
  showCSV (Just a) = showCSV a

instance ShowCSV SPitch where
  showCSV a = showNotationT a

main :: IO ()
main = do
  let input = getContents
      output = putStrLn
  txt <- input
  let xml = parseWithoutIds txt
  P.runEffect $ P.for (produceCSV $ notesToFrame $ xmlNotesHeard xml) (P.lift . output)
