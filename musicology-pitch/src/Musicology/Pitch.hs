{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Musicology.Pitch
  ( IntervalClass(..)
  , Interval(..)
  , oct, iabs
  -- , ClassyInterval(..)
  , Diatonic(..), Chromatic(..)
  , aug, dim, down, minor, major
  , VectorSpace(..), AdditiveGroup(..)
  , MidiInterval, MidiIC(..), mic
  , SInterval(..), SIC(..), sic
  , unison, second, third, fourth, tritone, fifth, sixth, seventh
  , unison', second', third', fourth', tritone', fifth', sixth', seventh'
  , Pitch(..), toPitch, toInterval, pto, pfrom, (+^), (^+), (-^), pc
  , MidiPitch, MidiPC, SPitch, SPC, spelled, spc
  , flt, shp, nat
  , c, d, e, f, g, a, b
  , c', d', e', f', g', a', b'
  , Notation(..)
  , transpose, embedI, embedP, embed, embed'
  ) where

import Control.Applicative ((<|>))

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Data.VectorSpace

import Data.Aeson
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Text.ParserCombinators.ReadP as R
import Data.Maybe (listToMaybe, maybe)
import Data.Char (isDigit)
import qualified Data.List as L

---------------
-- Intervals --
---------------

-- should pitches always be nums?
-- does multiplication always make sense?
-- scalar multiplication? vector space?

class (Interval i, Interval (IOf i), VectorSpace i, ICOf (IOf i) ~ i) => IntervalClass i where
  type IOf i
--  ic :: IOf i -> i
  emb :: i -> IOf i

class VectorSpace i => Interval i where
  type ICOf i
  ic :: i -> ICOf i
  toMidi :: i -> MidiInterval
  toFreq :: i -> Double
  toFreq i = 440 * 2 ** ((fromIntegral (toMidi i) - 9) / 12)
  octave :: Int -> i
  direction :: i -> Ordering
  default direction :: Ord i => i -> Ordering
  direction i = compare i zeroV

oct :: IntervalClass i => Int -> i -> IOf i
oct octs ic = emb ic ^+^ octave octs

iabs :: Interval i => i -> i
iabs i | direction i == LT = negateV i
       | otherwise         = i

-- class (Interval i, IntervalClass (PCOf i), IOf (ICOf i) ~ i) => ClassyInterval i where  

class Interval i => Diatonic i where
  isStep :: i -> Bool

class Interval i => Chromatic i where
  chromaticSemitone :: i

dim :: Chromatic i => i -> i
dim = (^-^chromaticSemitone)

aug :: Chromatic i => i -> i
aug = (^+^chromaticSemitone)

class Notation i where
  showNotation  :: i -> String
  showNotationT :: i -> T.Text
  showNotationT = T.pack . showNotation
  parseNotation :: R.ReadP i
  readNotation  :: String -> Maybe i
  readNotation str = fst <$> (listToMaybe $ R.readP_to_S parseFull str)
    where parseFull = do
            result <- parseNotation
            R.eof
            pure result
  readNotationT :: T.Text -> Maybe i
  readNotationT = readNotation . T.unpack

-- MidiInterval
---------------

newtype MidiIC = MidiIC Int
  deriving (Eq, Ord, NFData, Show)

mic :: Int -> MidiIC
mic = MidiIC . flip mod 12

instance AdditiveGroup MidiIC where
  zeroV = MidiIC 0
  negateV (MidiIC m) = mic $ negate m
  (MidiIC a) ^+^ (MidiIC b) = mic $ a + b
  (MidiIC a) ^-^ (MidiIC b) = mic $ a - b

instance VectorSpace MidiIC where
  type Scalar MidiIC = Int
  s *^ (MidiIC i) = mic (s*i)

instance Interval MidiIC where
  type ICOf MidiIC = MidiIC
  ic = id
  toMidi (MidiIC i) = i+60
  octave x = mic 0
  direction (MidiIC 0) = EQ
  direction (MidiIC i) = if i == 0
                         then EQ
                         else compare 6 i

instance IntervalClass MidiIC where
  type IOf MidiIC = MidiInterval
  -- ic = mic
  emb (MidiIC i) = i

instance Diatonic MidiIC where
  isStep (MidiIC i) = i <= 2 || i < 12 && i > 9 -- no i < 12?

instance Chromatic MidiIC where
  chromaticSemitone = MidiIC 1


-- helper
parseInt :: R.ReadP Int
parseInt = do
  sign <- R.option "" $ R.string "-"
  dgts <- R.munch1 isDigit
  pure $ read $ sign <> dgts

parseInt' :: R.ReadP Int
parseInt' = read <$> R.munch isDigit

munchChar :: Char -> R.ReadP String
munchChar c = R.munch (==c)

munchChar1 :: Char -> R.ReadP String
munchChar1 c = R.munch1 (==c)

anyChar :: [Char] -> R.ReadP Char
anyChar chars = R.satisfy (`elem` chars)

instance Notation MidiIC where
  showNotation (MidiIC i) = "ic" <> show i
  parseNotation = R.string "ic" >> mic <$> parseInt

type MidiInterval = Int
--  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance Interval Int where
  type ICOf Int = MidiIC
  ic = mic
  toMidi = id
  toFreq i = 440 * 2 ** ((fromIntegral i - 69)/12)
  octave = (12*)
--  direction i = compare i 0
--  icInt int = if int > 6 then int - 12 else int

-- instance ClassyInterval Int where
--   type PCOf Int = MidiIC

instance Diatonic Int where
  isStep a = (abs a) <= 2

instance Chromatic Int where
  chromaticSemitone = 1

instance Notation MidiInterval where
  showNotation = show
  parseNotation = parseInt
  readNotation = readMaybe


-- Spelled Interval
-------------------

-- data NoteNames = A | B | C | D | E | F | G
--   deriving (Eq, Ord, Show)

-- spelled interval type and instances

diachrom = [0, 2, 4, 5, 7, 9, 11]
dia2chrom d = (diachrom !! (mod d 7)) + 12 * (div d 7)
dianames = ["C", "D", "E", "F", "G", "A", "B"]
-- diaints  = ["uni", "2nd", "3rd", "4th", "5th", "6th", "7th"]
diafifths = [0,2,4,-1,1,3,5]
diaget lst  = (lst!!) . (flip mod 7)
perfectdia = [0,3,4]
accstr 0 _ _ = ""
accstr n u d | n > 0     = take n $ repeat u
             | otherwise = take (abs n) $ repeat d
qualpf n a p d | n > 0     = take n $ repeat a
               | n == 0    = [p]
               | otherwise = take (-n) $ repeat d
qualimpf n a mj mn d | n > 0     = take n $ repeat a
                     | n == 0    = [mj]
                     | n == (-1) = [mn]
                     | otherwise = take ((-n)-1) $ repeat d

data SInterval = SInterval
                 { dSteps :: Int
                 , cSteps :: Int
                 }
  deriving (Ord, Eq, Generic, Show)

instance ToJSON SInterval -- TODO make more specific
instance FromJSON SInterval

down x = negateV x

minor int = int 0
major int = int 1

unison    = SInterval 0 0
second  x = SInterval 1 (1+x)
third   x = SInterval 2 (3+x)
fourth    = SInterval 3 5
tritone   = aug fourth -- SInterval 3 6
fifth     = SInterval 4 7
sixth   x = SInterval 5 (8+x)
seventh x = SInterval 6 (10+x)

instance NFData SInterval

-- instance Show SInterval where
--   -- show (SInterval d c) = diaget diaints d <> accstr augs '+' '-' <> show (div d 7)
--   --   where augs = c - dia2chrom d
--   show (SInterval d c) = "SInterval " <> show d <> " " <> show c

instance AdditiveGroup SInterval where
  zeroV = SInterval 0 0
  negateV (SInterval d c) = SInterval (-d) (-c)
  (SInterval d1 c1) ^+^ (SInterval d2 c2) = SInterval (d1+d2) (c1+c2)
  (SInterval d1 c1) ^-^ (SInterval d2 c2) = SInterval (d1-d2) (c1-c2)

instance VectorSpace SInterval where
  type Scalar SInterval = Int
  s *^ (SInterval d c) = SInterval (s*d) (s*c)

instance Interval SInterval where
  type ICOf SInterval = SIC
  ic (SInterval d c) = sic d c
  toMidi (SInterval d c) = c + 12
  octave n = SInterval (7*n) (12*n)
  direction (SInterval d _) = compare d 0 -- is this a good idea?

-- instance ClassyInterval SInterval where
--   type ICOf SInterval = SIC

instance Diatonic SInterval where
  isStep (SInterval d _) = abs d < 2

instance Chromatic SInterval where
  chromaticSemitone = SInterval 0 1

-- helpers:

diffAug :: R.ReadP (Bool -> R.ReadP Int)
diffAug = do
  as <- R.munch1 (=='a')
  pure $ \_ -> pure $ length as

diffDim :: R.ReadP (Bool -> R.ReadP Int)
diffDim = do
  ds <- R.munch1 (=='d')
  pure $ \pf -> pure $ (0 - length ds) - if pf then 0 else 1

diffQual :: R.ReadP (Bool -> R.ReadP Int)
diffQual = do
  qual <- anyChar "MPm"
  case qual of
    'P' -> pure $ \pf -> if pf then pure 0 else R.pfail
    'M' -> pure $ \pf -> if pf then R.pfail else pure 0
    'm' -> pure $ \pf -> if pf then R.pfail else pure (-1)

instance Notation SInterval where
  showNotation i@(SInterval d c)
    | direction i == LT = "-" <> showNotation (negateV i)
    | otherwise = qual <> show (dia+1) <> octstr
    where dia = mod d 7
          diff = c - dia2chrom d
          qual = if dia `elem` perfectdia
                 then qualpf diff 'a' 'P' 'd'
                 else qualimpf diff 'a' 'M' 'm' 'd'
          oct = div d 7
          octstr = (if oct >= 0 then "+" else "") <> show oct
  parseNotation = do
    sign <- R.option '+' (R.char '-')
    fdif <- diffQual <|> diffAug <|> diffDim
    dia  <- (\x -> x-1) <$> parseInt'
    diff <- fdif (dia `elem` perfectdia)
    let chrom = dia2chrom dia + diff
    osgn <- R.char '+' <|> R.char '-'
    octi <- parseInt'
    let octs = if osgn == '-' then -octi else octi
        intv = SInterval (dia + 7*octs) (chrom + 12*octs)
    pure $ if sign == '-' then negateV intv else intv

-- spelled pitch class (aka tonal pc) type and instances
-- spc are based on the line of fifth

data SIC = SIC { sFifth :: Int }
  deriving (Ord, Eq, Show, Generic)

instance ToJSON SIC -- TODO: better keys in object
instance FromJSON SIC

-- sic d c = SIC (mod d 7) (mod c 12)
sic d c = SIC $ (diaget diafifths d) + 7*diff
  where diff = c - dia2chrom d

unison'    = sic 0 0
second'  x = sic 1 (1+x)
third'   x = sic 2 (3+x)
fourth'    = sic 3 5
tritone'   = aug fourth' -- sic 3 6
fifth'     = sic 4 7
sixth'   x = sic 5 (8+x)
seventh' x = sic 6 (10+x)

instance NFData SIC

-- instance Show SIC where
--   show (SIC d c) = diaget diaints d <> accstr augs '+' '-'
--     where augs = c - dia2chrom d

instance AdditiveGroup SIC where
  zeroV = SIC 0
  negateV (SIC f) = SIC (-f)
  (SIC f1) ^+^ (SIC f2) = SIC $ f1 + f2
  (SIC f1) ^-^ (SIC f2) = SIC $ f1 - f2

instance VectorSpace SIC where
  type Scalar SIC = Int
  s *^ (SIC f) = SIC $ f*s

instance Interval SIC where
  type ICOf SIC = SIC
  ic = id
  toMidi (SIC f) = mod (f*7) 12
  octave n = zeroV
  direction (SIC 0) = EQ
  direction i       = if d == 0 then EQ else if d < 4 then GT else LT
    where (SInterval d _) = emb i

instance Diatonic SIC where
  isStep ic = abs d < 2
    where (SInterval d _) = embedI fifth' (down fifth) ic

instance Chromatic SIC where
  chromaticSemitone = sic 0 1

instance IntervalClass SIC where
  type IOf SIC = SInterval
  -- ic (SInterval d c) = sic d c
  emb (SIC f) = SInterval (dia `mod` 7) (chrom - (12 * (dia `div` 7)))
    where dia   = f * 4
          chrom = f * 7

instance Notation SIC where
  showNotation i = qual <> show (d+1)
    where (SInterval d c) = emb i
          diff = c - dia2chrom d
          qual = if d `elem` perfectdia
                 then qualpf diff 'a' 'P' 'd'
                 else qualimpf diff 'a' 'M' 'm' 'd'
  parseNotation = do
    sign <- R.option '+' (R.char '-')
    fdif <- diffQual <|> diffAug <|> diffDim
    dia  <- (\x -> x-1) <$> parseInt'
    diff <- fdif (dia `elem` perfectdia)
    let chrom = dia2chrom dia + diff
    let intv = sic dia chrom
    pure $ if sign == '-' then negateV intv else intv

-------------
-- Pitches --
-------------

-- wrapper type: turn intervals into pitches
newtype Pitch a = Pitch a
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance NFData a => NFData (Pitch a)

instance Functor Pitch where
  fmap f (Pitch p) = Pitch (f p)

toPitch = Pitch
toInterval (Pitch i) = i

(Pitch a) `pto` (Pitch b) = b ^-^ a
(Pitch a) `pfrom` (Pitch b) = a ^-^ b
(Pitch p) +^ i = Pitch (p ^+^ i)
i ^+ (Pitch p) = Pitch (p ^+^ i)
(Pitch p) -^ i = Pitch (p ^-^ i)
pc :: (Interval p) => Pitch p -> Pitch (ICOf p)
pc = fmap ic

-- midi pitch
-------------

type MidiPitch = Pitch MidiInterval
type MidiPC    = Pitch MidiIC

midip  :: Int -> MidiPitch
midip = Pitch

midipc :: Int -> MidiPC
midipc = Pitch . mic

instance Notation MidiPitch where
  showNotation (Pitch i) = "p" <> showNotation i
  parseNotation = R.char 'p' >> (midip <$> parseInt)

instance Show MidiPitch where
  show = showNotation

instance Notation MidiPC where
  showNotation (Pitch i) = "pc" <> showNotation i
  parseNotation = R.string "pc" >> (midipc <$> parseInt)

instance Show MidiPC where
  show = showNotation

-- spelled pitch / pitch class
------------------------------

type SPitch = Pitch SInterval
type SPC    = Pitch SIC

spelled :: Int -> Int -> SPitch
spelled d c = Pitch $ SInterval d c

-- helper
parseAccs :: R.ReadP Int
parseAccs = R.option 0 $ sharps <|> flats
  where sharps = length <$> (munchChar1 '♯' <|> munchChar1 '#')
        flats  = negate . length <$> (munchChar1 '♭' <|> munchChar1 'b')

newtype Accidental = Acc Int
runAcc (Acc x) = x

flt = Acc (-1)
shp = Acc 1
nat = Acc 0

toSpelled :: Int -> Int -> Accidental -> Int -> SPitch
toSpelled dia chrom acc oct =
  spelled (dia + 7*oct) (chrom + (runAcc acc) + 12*oct)

c = toSpelled 0 0
d = toSpelled 1 2
e = toSpelled 2 4
f = toSpelled 3 5
g = toSpelled 4 7
a = toSpelled 5 9
b = toSpelled 6 11

instance Show SPitch where
  show = showNotation

instance Notation SPitch where
  showNotation (Pitch (SInterval d c)) =
    diaget dianames d <> accstr accs '♯' '♭' <> show (div d 7)
    where accs = c - dia2chrom d
  parseNotation = do
    name <- anyChar "ABCDEFG"
    dia  <- maybe R.pfail pure (L.elemIndex [name] dianames)
    diff <- parseAccs
    let chrom = dia2chrom dia + diff
    octs <- parseInt
    pure $ spelled (dia + 7*octs) (chrom + 12*octs)

spc :: Int -> Int -> SPC
spc d c = Pitch $ sic d c

toSPC :: Int -> Int -> Accidental -> SPC
toSPC dia chrom acc = spc dia (chrom + (runAcc acc))

c' = toSPC 0 0
d' = toSPC 1 2
e' = toSPC 2 4
f' = toSPC 3 5
g' = toSPC 4 7
a' = toSPC 5 9
b' = toSPC 6 11

instance Show SPC where
  show = showNotation

instance Notation SPC where
  showNotation (Pitch i) =
    diaget dianames d <> accstr accs '♯' '♭'
    where (SInterval d c) = emb i
          accs = c - dia2chrom d
  parseNotation = do
    name <- anyChar "ABCDEFG"
    dia  <- maybe R.pfail pure (L.elemIndex [name] dianames)
    diff <- parseAccs
    let chrom = dia2chrom dia + diff
    pure $ spc dia chrom

-------------
-- Helpers --
-------------

transpose :: (Functor f, Interval i) => i -> f i -> f i
transpose by = fmap (^+^ by)

embedI rot trans = (^+^ trans) . emb . (^+^ rot)

embedP rot trans = (+^ trans) . (fmap emb) . (+^ rot)

embed rot trans = transpose trans . (fmap emb) . transpose rot
embed' slide c0 = embed (negateV (ic slide)) (toInterval c0 ^+^ slide)
