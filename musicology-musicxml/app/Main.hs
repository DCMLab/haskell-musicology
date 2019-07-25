{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Musicology.MusicXML
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

main :: IO ()
main = do
  let input = getContents
      output = putStrLn
  txt <- input
  P.runEffect $ P.for (produceCSV $ notesToFrame $ xmlNotesHeard txt) (P.lift . output)
