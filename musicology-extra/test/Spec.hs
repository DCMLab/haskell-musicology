import Test.Hspec
import Musicology.Types
import Test.QuickCheck

-- some instances

instance Arbitrary SInterval where
  arbitrary = SInterval <$> arbitrary <*> arbitrary

instance Arbitrary SIC where
  arbitrary = SIC <$> arbitrary

instance Arbitrary MidiIC where
  arbitrary = mic <$> arbitrary

-- actual tests

main :: IO ()
main = hspec $ do
  testSIntervals
  testSIC

negateOrd :: Ordering -> Ordering
negateOrd LT = GT
negateOrd EQ = EQ
negateOrd GT = LT

testSIntervals = describe "SInterval" $ do
  describe "zeroV" $ do
    it "returns a perfect unison" $
      zeroV `shouldBe` unison
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
  describe "notation" $
    it "returns input on readN . showN" $ property $ \i ->
      readNotation (showNotation i) `shouldBe` Just (i :: SInterval)

testSIC = describe "SIC" $ do
  describe "octave" $ do
    it "is equal to zeroV" $ property $ \n ->
      zeroV `shouldBe` (octave n :: SIC)
    it "is the same for any number of octaves" $ property $ \n ->
      octave 1 `shouldBe` (octave n :: SIC)
  describe "ic / emb" $ do
    it "are id for ic . emb" $ property $ \i ->
      ic (emb $ i) `shouldBe` (i :: SIC)
    it "ignore octave changes in SInterval space" $ property $ \i n ->
      ic i `shouldBe` ic (i ^+^ octave n :: SIC)
  describe "direction" $ do
    it "goes up for 4ths" $ do
      direction fourth' `shouldBe` GT
      direction (aug fourth') `shouldBe` GT
    it "goes down for 5ths" $ do
      direction fifth' `shouldBe` LT
      direction (dim fifth') `shouldBe` LT
    it "is neutral for unisons" $ do
      direction unison' `shouldBe` EQ
      direction (aug unison') `shouldBe` EQ
      direction (dim unison') `shouldBe` EQ
  describe "notation" $
    it "returns input on readN . showN" $ property $ \i ->
      readNotation (showNotation i) `shouldBe` Just (i :: SIC)
