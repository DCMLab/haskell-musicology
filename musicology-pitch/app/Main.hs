module Main where

import           Data.Foldable                  ( forM_ )
import           Data.Maybe                     ( catMaybes )
import           Musicology.Pitch

-- do something with a list of pitches
transposeAndAddOctave :: Interval i => [Pitch i] -> i -> [Pitch i]
transposeAndAddOctave pitches by = map (\p -> p +^ by +^ octave) pitches

main :: IO ()
main = do
  -- perform the algorithm with MIDI pitches
  putStrLn "MIDI:"
  let midiPitches = midip <$> [60, 64, 67]
  forM_ (transposeAndAddOctave midiPitches 3)
    $ \p -> putStrLn $ "- " <> showNotation p

  -- perform the algorithm with MIDI pitch classes
  putStrLn "MIDI classes:"
  let midiPitches = midipc <$> [0, 4, 7]
  forM_ (transposeAndAddOctave midiPitches (mic 3))
    $ \p -> putStrLn $ "- " <> showNotation p

  -- perform the algorithm with spelled pitches
  putStrLn "spelled:"
  let spelledPitches :: [SPitch]
      spelledPitches = catMaybes $ readNotation <$> ["C4", "Eb4", "G#4", "C5"]
      Just interval  = readNotation "M3:0"
  forM_ (transposeAndAddOctave spelledPitches interval)
    $ \p -> putStrLn $ "- " <> showNotation p
