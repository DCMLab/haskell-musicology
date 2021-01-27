import Musicology.Pitch

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec $ do
  testSpelled


unsafeJust (Just x) = x

rsi :: String -> SInterval
rsi = unsafeJust . readNotation
rsic :: String -> SIC
rsic = unsafeJust . readNotation
rsp :: String -> SPitch
rsp = unsafeJust . readNotation
rspc :: String -> SPC
rspc = unsafeJust . readNotation

testSpelled = describe "Spelled" $ do
  it "can be constructed through constructors" $ do
    spelled 2 3 `shouldBe` SInterval 2 3
    spelledp 0 4 `shouldBe` Pitch (SInterval 0 4)
    sic (-3) `shouldBe` SIC (-3)
    spc 3 `shouldBe` Pitch (SIC 3)

  it "can be created through named intervals" $ do
    unison `shouldBe` spelled 0 0
    minor second `shouldBe` spelled (-5) 3
    major second `shouldBe` spelled 2 (-1)
    minor third `shouldBe` spelled (-3) 2
    major third `shouldBe` spelled 4 (-2)
    fourth `shouldBe` spelled (-1) 1
    tritone `shouldBe` spelled 6 (-3)
    fifth `shouldBe` spelled 1 0
    minor sixth `shouldBe` spelled (-4) 3
    major sixth `shouldBe` spelled 3 (-1)
    minor seventh `shouldBe` spelled (-2) 2
    major seventh `shouldBe` spelled 5 (-2)
    octave `shouldBe` spelled 0 1
    chromaticSemitone `shouldBe` spelled 7 (-4)
    
    unison `shouldBe` sic 0
    minor second' `shouldBe` sic (-5)
    major second' `shouldBe` sic 2
    minor third' `shouldBe` sic (-3)
    major third' `shouldBe` sic 4
    fourth' `shouldBe` sic (-1)
    tritone' `shouldBe` sic 6
    fifth' `shouldBe` sic 1
    minor sixth' `shouldBe` sic (-4)
    major sixth' `shouldBe` sic 3
    minor seventh' `shouldBe` sic (-2)
    major seventh' `shouldBe` sic 5
    octave `shouldBe` sic 0
    chromaticSemitone `shouldBe` sic 7

  it "can be parsed from strings" $ do
    readNotation "M3:1" `shouldBe` Just (spelled 4 (-1))
    readNotation "-M3:0" `shouldBe` Just (spelled (-4) 2)
    readNotation "C♭4" `shouldBe` Just (spelledp (-7) 8)
    readNotation "Cb4" `shouldBe` Just (spelledp (-7) 8)
    readNotation "m3" `shouldBe` Just (sic (-3))
    readNotation "-m3" `shouldBe` Just (sic 3)
    readNotation "C♯" `shouldBe` Just (spc 7)
    readNotation "C#" `shouldBe` Just (spc 7)
      
  it "accessors (positive SI)" $ do
    octaves (rsi "M3:1") `shouldBe` 1
    internalOctaves (rsi "M3:1") `shouldBe` -1
    fifths (rsi "M3:1") `shouldBe` 4
    degree (rsi "M3:1") `shouldBe` 2
    generic (rsi "M3:1") `shouldBe` 2
    diasteps (rsi "M3:1") `shouldBe` 9
    alteration (rsi "M3:1") `shouldBe` 0
    
  it "accessors (negative SI)" $ do
    octaves (rsi "-M3:1") `shouldBe` -2
    internalOctaves (rsi "-M3:1") `shouldBe` 1
    fifths (rsi "-M3:1") `shouldBe` -4
    degree (rsi "-M3:1") `shouldBe` 5
    generic (rsi "-M3:1") `shouldBe` -2
    diasteps (rsi "-M3:1") `shouldBe` -9
    alteration (rsi "-M3:1") `shouldBe` 0
    
  it "accessors (SIC)" $ do
    octaves (rsic "a5") `shouldBe` 0
    internalOctaves (rsic "a5") `shouldBe` 0
    fifths (rsic "a5") `shouldBe` 8
    degree (rsic "a5") `shouldBe` 4
    generic (rsic "a5") `shouldBe` 4
    diasteps (rsic "a5") `shouldBe` 4
    alteration (rsic "a5") `shouldBe` 1

  it "interval interface" $ do
    minor third ^+^ major third `shouldBe` fifth
    minor third ^+^ major seventh `shouldBe` major second ^+^ octave
    minor third ^+^ major seventh `shouldBe` major second ^+^ octave
    fifth ^+^ fifth `shouldBe` major second ^+^ octave
