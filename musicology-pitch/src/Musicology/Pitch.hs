{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module: Musicology.Pitch
Description: Representations and operations for musical pitch.
Copyright: Christoph Finkensiep, 2021
License: BSD
Maintainer: chfin@chfin.de
Stability: experimental

This package provides useful representations and operations for working with musical pitch.
Follows the same API and notation formats as the companion libraries:

- [Pitches.jl](https://github.com/DCMLab/Pitches.jl/) (Julia)
- [purescript-pitches](https://github.com/DCMLab/purescript-pitches) (Purescript)
- [rust-pitches](https://github.com/DCMLab/rust-pitches) (Rust)
- [pitchtypes](https://github.com/DCMLab/pitchtypes) (Python)


The two main goals are:

- providing types and operations (such as arithmetics, printing and parsing) for common types of pitches and intervals
- providing a generic interface for writing code that is agnostic to the specific pitch or interval types.

It allows you to write generic algorithms that can then be applied to pitches and intervals in different formats:


> import           Data.Foldable                  ( forM_ )
> import           Data.Maybe                     ( catMaybes )
> import           Musicology.Pitch
> 
> -- do something with a list of pitches
> transposeAndAddOctave :: Interval i => [Pitch i] -> i -> [Pitch i]
> transposeAndAddOctave pitches by = map (\p -> p +^ by +^ octave) pitches
> 
> main :: IO ()
> main = do
>   -- perform the algorithm with MIDI pitches
>   putStrLn "MIDI:"
>   let midiPitches = midip <$> [60, 64, 67]
>   forM_ (transposeAndAddOctave midiPitches 3)
>     $ \p -> putStrLn $ "- " <> showNotation p
> 
>   -- perform the algorithm with MIDI pitch classes
>   putStrLn "MIDI classes:"
>   let midiPitches = midipc <$> [0, 4, 7]
>   forM_ (transposeAndAddOctave midiPitches (mic 3))
>     $ \p -> putStrLn $ "- " <> showNotation p
> 
>   -- perform the algorithm with spelled pitches
>   putStrLn "spelled:"
>   let spelledPitches :: [SPitch]
>       spelledPitches = catMaybes $ readNotation <$> ["C4", "Eb4", "G#4", "C5"]
>       Just interval  = readNotation "M3:0"
>   forM_ (transposeAndAddOctave spelledPitches interval)
>     $ \p -> putStrLn $ "- " <> showNotation p

Output:

> MIDI:
> - p75
> - p79
> - p82
> MIDI classes:
> - pcic3
> - pcic7
> - pcic10
> spelled:
> - E5
> - G5
> - B♯5
> - E6
-}
module Musicology.Pitch
  ( module Musicology.Pitch.Class
  , module Musicology.Pitch.Spelled
  , module Musicology.Pitch.Midi
  )
where

import           Musicology.Pitch.Class
import           Musicology.Pitch.Spelled
import           Musicology.Pitch.Midi
