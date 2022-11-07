module TestSpelled where

import           Musicology.Pitch

import           Test.Hspec
import           Test.QuickCheck

unsafeJust (Just x) = x

rsi :: String -> SInterval
rsi = unsafeJust . readNotation
rsic :: String -> SIC
rsic = unsafeJust . readNotation
rsp :: String -> SPitch
rsp = unsafeJust . readNotation
rspc :: String -> SPC
rspc = unsafeJust . readNotation

-- some instances

instance Arbitrary SInterval where
  arbitrary = SInterval <$> arbitrary <*> arbitrary

instance Arbitrary SIC where
  arbitrary = SIC <$> arbitrary

instance Arbitrary MidiIC where
  arbitrary = mic <$> arbitrary

negateOrd :: Ordering -> Ordering
negateOrd LT = GT
negateOrd EQ = EQ
negateOrd GT = LT

testSIntervalProps = describe "SInterval properties" $ do
  describe "zeroV" $ do
    it "returns a perfect unison" $ zeroV `shouldBe` (unison :: SInterval)
    it "is neutral wrt addition" $ property $ \i ->
      i ^+^ zeroV `shouldBe` (i :: SInterval)
  describe "negateV" $ do
    it "changes the direction of an interval" $ property $ \i ->
      direction (negateV i :: SInterval) `shouldBe` negateOrd (direction i)
  describe "^-^" $ do
    it "behaves inverse to ^+^" $ property $ \i1 i2 ->
      i1 ^+^ i2 ^-^ i2 `shouldBe` (i1 :: SInterval)
    it "returns zeroV for i ^-^ i" $ property $ \i ->
      (i :: SInterval) ^-^ i `shouldBe` zeroV
  describe "ordering" $ it "is symmetric to interval inversion" $ property $ \i1 i2 ->
      (i1 :: SInterval) < (i2 :: SInterval) `shouldBe` (down i1 > down i2)
  describe "notation" $ it "returns input on readN . showN" $ property $ \i ->
    readNotation (showNotation i) `shouldBe` Just (i :: SInterval)

testSICProps = describe "SIC properties" $ do
  describe "octave" $ do
    it "is equal to zeroV" $ property $ \n ->
      zeroV `shouldBe` (octave ^* n :: SIC)
    it "is the same for any number of octaves" $ property $ \n ->
      octave `shouldBe` (octave ^* n :: SIC)
  describe "ic / emb" $ do
    it "are id for ic . emb" $ property $ \i -> ic (emb i) `shouldBe` (i :: SIC)
    it "ignore octave changes in SInterval space" $ property $ \i n ->
      ic i `shouldBe` ic (i ^+^ octave ^* n :: SIC)
  describe "direction" $ do
    it "goes up for 4ths" $ do
      direction fourth' `shouldBe` GT
      direction (aug fourth') `shouldBe` GT
    it "goes down for 5ths" $ do
      direction fifth' `shouldBe` LT
      direction (dim fifth') `shouldBe` LT
    it "is neutral for unisons" $ do
      direction (unison :: SIC) `shouldBe` EQ
      direction (aug unison :: SIC) `shouldBe` GT
      direction (dim unison :: SIC) `shouldBe` LT
  describe "ordering" $ it "is symmetric to interval inversion" $ property $ \i1 i2 ->
      (i1 :: SIC) < (i2 :: SIC) `shouldBe` (down i1 > down i2)
  describe "notation" $ it "returns input on readN . showN" $ property $ \i ->
    readNotation (showNotation i) `shouldBe` Just (i :: SIC)

testSpelled = describe "Spelled" $ do
  testSIntervalProps
  testSICProps

  it "constructors" $ do
    spelled 2 3 `shouldBe` SInterval 2 3
    spelledDiaChrom 3 5 `shouldBe` SInterval (-1) 1
    spelledp 0 4 `shouldBe` Pitch (SInterval 0 4)
    sic (-3) `shouldBe` SIC (-3)
    spc 3 `shouldBe` Pitch (SIC 3)

  it "named intervals" $ do
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

  it "string notation parsing" $ do
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

  it "accessors (SPitch)" $ do
    octaves (rsp "Ebb5") `shouldBe` 5
    fifths (rsp "Ebb5") `shouldBe` (-10)
    degree (rsp "Ebb5") `shouldBe` 2
    alteration (rsp "Ebb5") `shouldBe` (-2)
    letter (rsp "Ebb5") `shouldBe` 'E'

  it "accessors (SPC)" $ do
    octaves (rspc "F#") `shouldBe` 0
    fifths (rspc "F#") `shouldBe` 6
    degree (rspc "F#") `shouldBe` 3
    alteration (rspc "F#") `shouldBe` 1
    letter (rspc "F#") `shouldBe` 'F'

  it "accessors (edge cases)" $ do
    alteration (rsic "P4") `shouldBe` 0
    alteration (rsic "M7") `shouldBe` 0
    alteration (rsi "-P4:0") `shouldBe` 0
    alteration (rsi "-M7:0") `shouldBe` 0

    alteration (rsic "a4") `shouldBe` 1
    alteration (rsic "m7") `shouldBe` (-1)
    alteration (rsi "-a4:0") `shouldBe` 1
    alteration (rsi "-m7:0") `shouldBe` (-1)

    alteration (rsic "d1") `shouldBe` (-1)
    alteration (rsi "d1:0") `shouldBe` 1 -- d1:0 == -a1:0

    alteration (rspc "F") `shouldBe` 0
    alteration (rspc "B") `shouldBe` 0
    alteration (rsp "Cb-1") `shouldBe` (-1)

  it "printing notation" $ do
    showNotation (rsi "m3:1") `shouldBe` "m3:1"
    showNotation (rsp "Eb4") `shouldBe` "E♭4"
    showNotation (rsic "m3") `shouldBe` "m3"
    showNotation (rspc "E#") `shouldBe` "E♯"

  describe "Interval Interface" $ do
    it "addition / subtraction / negation" $ do
      rsi "m3:0" ^+^ rsi "M3:0" `shouldBe` rsi "P5:0"
      rsi "m3:0" ^+^ rsi "M7:0" `shouldBe` rsi "M2:1"
      rsi "P5:0" ^+^ rsi "P5:0" `shouldBe` rsi "M2:1"
      rsi "-m3:0" ^+^ rsi "M3:0" `shouldBe` rsi "a1:0"
      rsi "m3:0" ^+^ rsi "-M3:0" `shouldBe` rsi "-a1:0"
      rsi "m3:0" ^-^ rsi "M3:0" `shouldBe` rsi "-a1:0"
      rsi "m3:0" ^-^ rsi "M6:0" `shouldBe` rsi "-a4:0"
      negateV (rsi "P4:0") `shouldBe` rsi "-P4:0"
      negateV (rsi "P4:0") `shouldBe` rsi "P5:-1"
      negateV (rsi "P5:0") `shouldBe` unison ^-^ rsi "P5:0"

    it "multiplication" $ do
      rsi "P5:0" ^* 2 `shouldBe` rsi "M2:1"
      rsi "M2:0" ^* 4 `shouldBe` rsi "a5:0"
      rsi "-m3:0" ^* 4 `shouldBe` rsi "-d2:1"
      rsi "M3:0" ^* (-3) `shouldBe` rsi "-a7:0"
      4 *^ rsi "M2:0" `shouldBe` rsi "a5:0"
      4 *^ rsi "-M3:0" `shouldBe` rsi "-aa2:1"
      5 *^ rsi "M3:0" `shouldBe` rsi "aaa4:1"

    it "ordering" $ do
      direction (rsi "m2:0") `shouldBe` GT
      direction (rsi "P1:0") `shouldBe` EQ
      direction (rsi "a1:0") `shouldBe` GT
      direction (rsi "d1:0") `shouldBe` LT
      direction (rsi "-m3:0") `shouldBe` LT
      direction (rsi "P4:0") `shouldBe` GT
      direction (rsi "-M7:0") `shouldBe` LT
      iabs (rsi "-m3:0") `shouldBe` rsi "m3:0"
      (rsi "m2:0" < rsi "M2:0") `shouldBe` True
      (rsi "-m2:0" > rsi "-M2:0") `shouldBe` True

    it "interval class conversion" $ do
      ic (rsi "M3:3") `shouldBe` rsic "M3"
      ic (rsi "-M3:1") `shouldBe` rsic "m6"

    it "steps (true)" $ do
      isStep (rsi "d1:0") `shouldBe` True
      isStep (rsi "P1:0") `shouldBe` True
      isStep (rsi "a1:0") `shouldBe` True
      isStep (rsi "d2:0") `shouldBe` True
      isStep (rsi "m2:0") `shouldBe` True
      isStep (rsi "M2:0") `shouldBe` True
      isStep (rsi "a2:0") `shouldBe` True
      isStep (rsi "-d2:0") `shouldBe` True
      isStep (rsi "-m2:0") `shouldBe` True
      isStep (rsi "-M2:0") `shouldBe` True
      isStep (rsi "-a2:0") `shouldBe` True

    it "steps (false)" $ do
      isStep (rsi "d3:0") `shouldBe` False
      isStep (rsi "-d3:0") `shouldBe` False
      isStep (rsi "M7:0") `shouldBe` False
      isStep (rsi "-M7:0") `shouldBe` False
      isStep (rsi "P1:1") `shouldBe` False
      isStep (rsi "-P1:1") `shouldBe` False
      isStep (rsi "m2:1") `shouldBe` False
      isStep (rsi "-m2:1") `shouldBe` False

  describe "Interval Class Interface" $ do
    it "addition / subtraction / negation" $ do
      rsic "m3" ^+^ rsic "M3" `shouldBe` rsic "P5"
      rsic "m3" ^+^ rsic "M7" `shouldBe` rsic "M2"
      rsic "P5" ^+^ rsic "P5" `shouldBe` rsic "M2"
      rsic "-m3" ^+^ rsic "M3" `shouldBe` rsic "a1"
      rsic "m3" ^+^ rsic "-M3" `shouldBe` rsic "-a1"
      rsic "m3" ^-^ rsic "M3" `shouldBe` rsic "-a1"
      rsic "m3" ^-^ rsic "M6" `shouldBe` rsic "-a4"
      negateV (rsic "P4") `shouldBe` rsic "-P4"
      negateV (rsic "P4") `shouldBe` rsic "P5"
      negateV (rsic "P5") `shouldBe` unison ^-^ rsic "P5"

    it "multiplication" $ do
      rsic "P5" ^* 2 `shouldBe` rsic "M2"
      rsic "M2" ^* 4 `shouldBe` rsic "a5"
      rsic "-m3" ^* 4 `shouldBe` rsic "-d2"
      rsic "M3" ^* (-3) `shouldBe` rsic "-a7"
      4 *^ rsic "M2" `shouldBe` rsic "a5"
      4 *^ rsic "-M3" `shouldBe` rsic "-aa2"
      5 *^ rsic "M3" `shouldBe` rsic "aaa4"

    it "ordering" $ do
      direction (rsic "m2") `shouldBe` GT
      direction (rsic "P1") `shouldBe` EQ
      direction (rsic "a1") `shouldBe` GT
      direction (rsic "d1") `shouldBe` LT
      direction (rsic "-m3") `shouldBe` LT
      direction (rsic "P4") `shouldBe` GT
      direction (rsic "M7") `shouldBe` LT
      iabs (rsic "-m3") `shouldBe` rsic "m3"

    it "interval conversion" $ do
      ic (rsic "M3") `shouldBe` rsic "M3"
      emb (rsic "M3") `shouldBe` rsi "M3:0"
      emb (rsic "m3") `shouldBe` rsi "m3:0"
      emb (rsic "M7") `shouldBe` rsi "M7:0"
      emb (rsic "a1") `shouldBe` rsi "a1:0"
      emb (rsic "d1") `shouldBe` rsi "d1:0"
      emb (rsic "P4") `shouldBe` rsi "P4:0"

    it "steps (true)" $ do
      isStep (rsic "d1") `shouldBe` True
      isStep (rsic "P1") `shouldBe` True
      isStep (rsic "a1") `shouldBe` True
      isStep (rsic "d2") `shouldBe` True
      isStep (rsic "m2") `shouldBe` True
      isStep (rsic "M2") `shouldBe` True
      isStep (rsic "a2") `shouldBe` True
      isStep (rsic "-d2") `shouldBe` True
      isStep (rsic "-m2") `shouldBe` True
      isStep (rsic "-M2") `shouldBe` True
      isStep (rsic "-a2") `shouldBe` True

    it "steps (false)" $ do
      isStep (rsic "d3") `shouldBe` False
      isStep (rsic "-d3") `shouldBe` False

  describe "Pitch Interface" $ do
    it "conversion" $ do
      toPitch (rsi "m3:4") `shouldBe` rsp "Eb4"
      toInterval (rsp "C#3") `shouldBe` rsi "a1:3"
      pc (rsp "Eb4") `shouldBe` rspc "Eb"

    it "arithmetics" $ do
      rsp "Eb4" +^ rsi "P5:0" `shouldBe` rsp "Bb4"
      rsp "Eb4" +^ rsi "-m3:0" `shouldBe` rsp "C4"
      rsp "Eb4" -^ rsi "P5:0" `shouldBe` rsp "Ab3"
      rsp "G4" `pfrom` rsp "C#4" `shouldBe` rsi "d5:0"
      rsp "G4" `pto` rsp "C#4" `shouldBe` rsi "-d5:0"
      rsp "C-1" > rsp "Cb-1" `shouldBe` True

  describe "Pitch Class Interface" $ do
    it "conversion" $ do
      toPitch (rsic "m3") `shouldBe` rspc "Eb"
      toInterval (rspc "E") `shouldBe` rsic "M3"
      pc (rspc "Eb") `shouldBe` rspc "Eb"
      emb <$> rspc "Eb" `shouldBe` rsp "Eb0"

    it "arithmetics" $ do
      rspc "Eb" +^ rsic "P5" `shouldBe` rspc "Bb"
      rspc "Eb" +^ rsic "-m3" `shouldBe` rspc "C"
      rspc "Eb" -^ rsic "P5" `shouldBe` rspc "Ab"
      rspc "G" `pfrom` rspc "C#" `shouldBe` rsic "d5"
      rspc "G" `pto` rspc "C#" `shouldBe` rsic "a4"
      rspc "C" > rspc "Cb" `shouldBe` True
