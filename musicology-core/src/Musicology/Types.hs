{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Musicology.Types
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
  , Timed(..), HasTime(..)
  , Pitched(..), HasInterval(..), HasPitch(..)
  , Identifiable(..)
  , Notation(..)
  , transpose, embedI, embedP, embed, embed'
  -- , PitchClassContainer(..), PitchContainer(..), embed, embed'
  -- , AbsPitchContainer(..)
  , TimedEvent(..), timedEventContent
  , Note(..), NoteId(..)
  , OnOff(..), isOn, isOff
  ) where

import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity(..))

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Data.VectorSpace

import Lens.Micro
import Lens.Micro.Extras

import qualified Euterpea.Music as EM
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

instance EM.ToMusic1 MidiIC where
  toMusic1 = EM.toMusic1 . EM.mMap (\i -> (emb i + 60))

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

instance EM.ToMusic1 (SInterval) where
  toMusic1 = EM.toMusic1 . EM.mMap toMidi

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

instance EM.ToMusic1 (SIC) where
  toMusic1 = EM.toMusic1 . EM.mMap ((+60) . toMidi)

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

instance EM.ToMusic1 p => EM.ToMusic1 (Pitch p) where
  toMusic1 = EM.toMusic1 . EM.mMap toInterval

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

----------------
-- Containers --
----------------

class (Num (TimeOf a), Ord (TimeOf a)) => Timed a where
  type TimeOf a

class Timed a => HasTime a where
  onsetL :: Lens' a (TimeOf a)
  onsetL = lens onset (flip setOnset)
  offsetL :: Lens' a (TimeOf a)
  offsetL = lens offset (flip setOffset)
  onset :: a -> TimeOf a
  onset = view onsetL
  offset :: a -> TimeOf a
  offset = view offsetL
  setOnset :: TimeOf a -> a -> a
  setOnset = set onsetL
  setOffset :: TimeOf a -> a -> a
  setOffset = set offsetL

class (Interval (IntervalOf a)) => Pitched a where
  type IntervalOf a

class Pitched a => HasInterval a where
  intervalL :: Lens' a (IntervalOf a)
  intervalL = lens interval (flip setInterval)
  interval :: a -> IntervalOf a
  interval = view intervalL
  setInterval :: IntervalOf a -> a -> a
  setInterval = set intervalL

class Pitched a => HasPitch a where
  pitchL :: Lens' a (Pitch (IntervalOf a))
  pitchL = lens pitch (flip setPitch)
  pitch :: a -> Pitch (IntervalOf a)
  pitch = view pitchL
  setPitch :: Pitch (IntervalOf a) -> a -> a
  setPitch = set pitchL

-- some container helpers (put in separate module?)

transpose :: (Functor f, Interval i) => i -> f i -> f i
transpose by = fmap (^+^ by)

embedI rot trans = (^+^ trans) . emb . (^+^ rot)

embedP rot trans = (+^ trans) . (fmap emb) . (+^ rot)

embed rot trans = transpose trans . (fmap emb) . transpose rot
embed' slide c0 = embed (negateV (ic slide)) (toInterval c0 ^+^ slide)

-- class (Pitch (PitchOf c), PitchContainer c) => AbsPitchContainer c where
--   refPitch :: c -> PitchOf c
--   transposeTo :: PitchOf c -> c -> c
--   transposeTo p c = transposeBy (p ^-^ refPitch c) c

-- some default instances
-------------------------

instance Interval p => Pitched [p] where
  type IntervalOf [p] = p

instance Interval p => Pitched (Maybe p) where
  type IntervalOf (Maybe p) = p
  
instance Interval p => Pitched (Identity p) where
  type IntervalOf (Identity p) = p

---------
-- IDs --
---------

class Identifiable i where
  type IdOf i
  getId :: i -> IdOf i

----------------
-- TimedEvent --
----------------

data TimedEvent c t = TimedEvent c t t
  deriving (Eq, Ord, Show, Read)

timedEventContent (TimedEvent c _ _) = c

instance (Num t, Ord t) => Timed (TimedEvent p t) where
  type TimeOf (TimedEvent p t) = t

instance (Num t, Ord t) => HasTime (TimedEvent p t) where
  onsetL f (TimedEvent e on off) = fmap (\on' -> TimedEvent e on' off) (f on)
  offsetL f (TimedEvent e on off) = fmap (\off' -> TimedEvent e on off') (f off)

instance Pitched c => Pitched (TimedEvent c t) where
  type IntervalOf (TimedEvent c t) = IntervalOf c

-- instance Pitch p => Pitched (TimedEvent p t) where
--   pitch (TimedEvent p _ _) = p

-- instance PitchContainer c => PitchContainer (TimedEvent c t) where
--   mapPitches f (TimedEvent c on off) = TimedEvent (mapPitches f c) on off
--   pitches (TimedEvent c _ _) = pitches c

----------
-- Note --
----------

data Note p t = Note !(Pitch p) !t !t
  deriving (Eq, Ord, Generic)

instance (NFData p, NFData t) => NFData (Note p t) 

deriving instance (Show (Pitch p), Show t) => Show (Note p t)
deriving instance (Read (Pitch p), Read t) => Read (Note p t)

instance (Num t, Ord t) => Timed (Note p t) where
  type TimeOf (Note p t) = t

instance (Num t, Ord t) => HasTime (Note p t) where
  onsetL f (Note p on off) = fmap (\on' -> Note p on' off) (f on)
  offsetL f (Note p on off) = fmap (\off' -> Note p on off') (f off)

instance Interval p => Pitched (Note p t) where
  type IntervalOf (Note p t) = p

instance Interval p => HasPitch (Note p t) where
  pitchL f (Note p on off) = fmap (\p' -> Note p' on off) (f p)

instance (ToJSON p, ToJSON t) => ToJSON (Note p t) where
  toJSON (Note (Pitch p) on off) =
    object ["pitch" .= p, "onset" .= on, "offset" .= off]
  toEncoding (Note (Pitch p) on off) =
    pairs ("pitch" .= p <> "onset" .= on <> "offset" .= off)

instance (FromJSON p, FromJSON t) => FromJSON (Note p t) where
  parseJSON = withObject "Note" $ \v -> Note
    <$> (toPitch <$> v .: "pitch")
    <*> v .: "onset"
    <*> v .: "offset"

-- note with id
---------------

data NoteId p t i = NoteId !(Pitch p) !t !t !i
  deriving (Eq, Ord, Generic)

instance (NFData p, NFData t, NFData i) => NFData (NoteId p t i)

deriving instance (Show (Pitch p), Show t, Show i) => Show (NoteId p t i)
deriving instance (Read (Pitch p), Read t, Read i) => Read (NoteId p t i)

instance (Num t, Ord t) => Timed (NoteId p t i) where
  type TimeOf (NoteId p t i) = t

instance (Num t, Ord t) => HasTime (NoteId p t i) where
  onsetL f (NoteId p on off id) = fmap (\on' -> NoteId p on' off id) (f on)
  offsetL f (NoteId p on off id) = fmap (\off' -> NoteId p on off' id) (f off)

instance Interval p => Pitched (NoteId p t i) where
  type IntervalOf (NoteId p t i) = p

instance Interval p => HasPitch (NoteId p t i) where
  pitchL f (NoteId p on off id) = fmap (\p' -> NoteId p' on off id) (f p)

instance Identifiable (NoteId p t i) where
  type IdOf (NoteId p t i) = i
  getId (NoteId _ _ _ i) = i

instance (ToJSON p, ToJSON t, ToJSON i) => ToJSON (NoteId p t i) where
  toJSON (NoteId (Pitch p) on off id) =
    object ["pitch" .= p, "onset" .= on, "offset" .= off, "id" .= id]
  toEncoding (NoteId (Pitch p) on off id) =
    pairs ("pitch" .= p <> "onset" .= on <> "offset" .= off <> "id" .= id)

instance (FromJSON p, FromJSON t, FromJSON i) => FromJSON (NoteId p t i) where
  parseJSON = withObject "Note" $ \v -> NoteId
    <$> (toPitch <$> v .: "pitch")
    <*> v .: "onset"
    <*> v .: "offset"
    <*> v .: "id"


----------------------------------------------
-- Ons and Offs                             --
-- - make onset and offset separate objects --
----------------------------------------------

data OnOff c t = Onset c !t
               | Offset c !t
  deriving (Eq, Ord, Show, Read, Generic)

isOn (Onset _ _) = True
isOn (Offset _ _) = False
ifOff (Onset _ _) = False
isOff (Offset _ _) = True

onOffContent :: Lens' (OnOff c t) c
onOffContent f (Onset c t) = fmap (\c' -> Onset c' t) (f c)
onOffContent f (Offset c t) = fmap (\c' -> Offset c' t) (f c)

instance (Num t, Ord t) => Timed (OnOff c t) where
  type TimeOf (OnOff c t) = t

instance (Num t, Ord t) => HasTime (OnOff p t) where
  onsetL f (Onset p t) = fmap (\t' -> Onset p t') (f t)
  onsetL f (Offset p t) = fmap (\t' -> Offset p t') (f t)
  offsetL = onsetL

instance Pitched c => Pitched (OnOff c t) where
  type IntervalOf (OnOff c t) = IntervalOf c

instance HasInterval c => HasInterval (OnOff c t) where
  intervalL = onOffContent . intervalL

instance HasPitch c => HasPitch (OnOff c t) where
  pitchL = onOffContent . pitchL

---------------------------------
-- contextual note: held over? --
---------------------------------
