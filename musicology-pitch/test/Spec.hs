import           Test.QuickCheck
import           Test.Hspec

import           TestSpelled                    ( testSpelled )

main :: IO ()
main = hspec $ do
  testSpelled
  -- TODO: test MIDI
