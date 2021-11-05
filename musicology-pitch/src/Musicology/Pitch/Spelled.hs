{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module: Musicology.Pitch.Spelled
Description: Spelled pitch and interval types (Western notation).
Copyright: Christoph Finkensiep, 2021
License: BSD
Maintainer: chfin@chfin.de
Stability: experimental

This module defines pitch and interval types for spelled pitch, i.e. Western notation.
-}
module Musicology.Pitch.Spelled
  ( -- * Interval Types
    SInterval(..)
  , spelled
  , spelledDiaChrom
  , SIC(..)
  , sic
  -- * Pitch Types
  , SPitch
  , SPC
  , spelledp
  , spc
  -- * Common Accessors
  , Spelled(..)
  , letter
  -- * Concrete Intervals
  --
  -- | Concrete intervals come in two variants,
  -- one for intervals (e.g. 'fifth') and one for interval classes ('fifth'').
  -- Imperfect intervals have a 'ImperfectInterval' type
  -- and must be used with 'major' or 'minor' (e.g. @'minor' 'third'@).
  -- All (fully applied) intervals can be used with 'aug', 'dim' and 'down'
  , second
  , third
  , fourth
  , tritone
  , fifth
  , sixth
  , seventh
  , second'
  , third'
  , fourth'
  , tritone'
  , fifth'
  , sixth'
  , seventh'
  -- * Concrete pitches
  --
  -- | Concrete pitches are constructed from an accidental ('flt', 'nat', or 'shp')
  -- and (for non-class pitches) and octave number.
  , Accidental(..)
  , flt
  , nat
  , shp
  , c
  , d
  , e
  , f
  , g
  , a
  , b
  , c'
  , d'
  , e'
  , f'
  , g'
  , a'
  , b'
  )
where

import           Musicology.Pitch.Class
import           Musicology.Pitch.Internal

import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Control.DeepSeq                ( NFData )
import qualified Text.ParserCombinators.ReadP  as R
import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( ord
                                                , chr
                                                )
import Data.Hashable (Hashable)

-- Spelled Interval
-------------------

-- helpers

isPerfect :: Int -> Bool
isPerfect 0 = True
isPerfect 3 = True
isPerfect 4 = True
isPerfect _ = False

accstr 0 _ _ = ""
accstr n u d | n > 0     = replicate n u
             | otherwise = replicate (abs n) d
qualpf n a p d | n > 0     = replicate n a
               | n == 0    = [p]
               | otherwise = replicate (-n) d
qualimpf n a mj mn d | n > 0     = replicate n a
                     | n == 0    = [mj]
                     | n == (-1) = [mn]
                     | otherwise = replicate ((-n) - 1) d

fifths2degree :: Int -> Int
fifths2degree fifths = fifths * 4 `mod` 7

-- accessor class (for SInterval and SIC)

class Spelled i where
  fifths :: i -> Int
  octaves :: i -> Int
  internalOctaves :: i -> Int
  degree :: i -> Int
  generic :: i -> Int
  diasteps :: i -> Int
  alteration :: i -> Int

data SInterval = SInterval
                 { siFifths :: Int
                 , siOctaves :: Int
                 }
  deriving (Eq, Generic, Show, NFData, Hashable)

-- smart constructors

spelled :: Int -> Int -> SInterval
spelled = SInterval

wholetone = spelled 2 (-1)

onlyDia x = wholetone ^* x ^-^ chromaticSemitone ^* (2 * x)

spelledDiaChrom :: Int -> Int -> SInterval
spelledDiaChrom dia chrom = diaPart ^+^ chromPart
 where
  diaPart   = wholetone ^* dia
  chromPart = chromaticSemitone ^* (chrom - 2 * dia)

second = Impf (spelled 2 (-1) ^-^)
third = Impf (spelled 4 (-2) ^-^)
fourth = spelled (-1) 1
tritone = aug fourth -- SInterval 3 6
fifth = spelled 1 0
sixth = Impf (spelled 3 (-1) ^-^)
seventh = Impf (spelled 5 (-2) ^-^)

instance ToJSON SInterval -- TODO make more specific
instance FromJSON SInterval

instance Spelled SInterval where
  fifths (SInterval f _) = f
  octaves (SInterval f o) = o + (f * 4 `div` 7)
  internalOctaves (SInterval _ o) = o
  degree (SInterval f _) = fifths2degree f
  generic i =
    if direction i == LT then negate $ degree $ negateV i else degree i
  diasteps (SInterval f o) = f * 4 + o * 7
  alteration i = (fifths (iabs i) + 1) `div` 7

instance Ord SInterval where
  i1 <= i2 = (diasteps i1, alteration i1) <= (diasteps i2, alteration i2)
  compare i1 i2 =
    compare (diasteps i1, alteration i1) (diasteps i2, alteration i2)

-- instance Show SInterval where
--   -- show (SInterval d c) = diaget diaints d <> accstr augs '+' '-' <> show (div d 7)
--   --   where augs = c - dia2chrom d
--   show (SInterval d c) = "SInterval " <> show d <> " " <> show c

instance AdditiveGroup SInterval where
  zeroV = SInterval 0 0
  negateV (SInterval f o) = SInterval (-f) (-o)
  (SInterval f1 o1) ^+^ (SInterval f2 o2) = SInterval (f1 + f2) (o1 + o2)
  (SInterval f1 o1) ^-^ (SInterval f2 o2) = SInterval (f1 - f2) (o1 - o2)

instance VectorSpace SInterval where
  type Scalar SInterval = Int
  s *^ (SInterval f o) = SInterval (s * f) (s * o)

instance Interval SInterval where
  type ICOf SInterval = SIC
  ic (SInterval f _) = sic f
  octave = SInterval 0 1
  direction i = compare (diasteps i) 0

instance Diatonic SInterval where
  isStep i = abs (diasteps i) < 2

instance Chromatic SInterval where
  chromaticSemitone = SInterval 7 (-4)

instance ToMidi SInterval where
  toMidi (SInterval f o) = f * 7 + o * 12

-- parsing helpers:

anyChar :: [Char] -> R.ReadP Char
anyChar chars = R.satisfy (`elem` chars)

altAug :: R.ReadP (Bool -> R.ReadP Int)
altAug = do
  as <- R.munch1 (== 'a')
  pure $ \_ -> pure $ length as

altDim :: R.ReadP (Bool -> R.ReadP Int)
altDim = do
  ds <- R.munch1 (== 'd')
  pure $ \pf -> pure $ negate (length ds) - if pf then 0 else 1

altQual :: R.ReadP (Bool -> R.ReadP Int)
altQual = do
  qual <- anyChar "MPm"
  case qual of
    'P' -> pure $ \pf -> if pf then pure 0 else R.pfail
    'M' -> pure $ \pf -> if pf then R.pfail else pure 0
    'm' -> pure $ \pf -> if pf then R.pfail else pure (-1)

parseDia = do
  falt <- altQual <|> altAug <|> altDim
  dia  <- (\x -> x - 1) <$> parseInt'
  alt  <- falt $ isPerfect dia
  return $ ((dia * 2 + 1) `mod` 7) - 1 + (7 * alt)

instance Notation SInterval where
  showNotation i | direction i == LT = "-" <> showNotation (negateV i)
                 | otherwise         = qual <> dia <> (':' : octs)
   where
    deg  = degree i
    dia  = show $ deg + 1
    alt  = alteration i
    qual = if isPerfect deg
      then qualpf alt 'a' 'P' 'd'
      else qualimpf alt 'a' 'M' 'm' 'd'
    octs = show $ octaves i
  parseNotation = do
    sign <- R.option '+' (R.char '-')
    f    <- parseDia
    R.char ':'
    o <- parseInt
    let i = SInterval f $ o - ((f * 4) `div` 7)
    pure $ if sign == '-' then negateV i else i

-- spelled pitch class (aka tonal pc) type and instances
-- spc are based on the line of fifth

newtype SIC = SIC { sFifth :: Int }
  deriving (Ord, Eq, Show, Generic, NFData, Hashable)

instance ToJSON SIC -- TODO: better keys in object
instance FromJSON SIC

sic :: Int -> SIC
sic = SIC

second' = Impf (sic 2 ^-^)
third' = Impf (sic 4 ^-^)
fourth' = sic (-1)
tritone' = sic 6
fifth' = sic 1
sixth' = Impf (sic 3 ^-^)
seventh' = Impf (sic 5 ^-^)

instance Spelled SIC where
  fifths (SIC f) = f
  octaves _ = 0
  internalOctaves _ = 0
  degree (SIC f) = fifths2degree f
  generic (SIC f) = fifths2degree f
  diasteps (SIC f) = fifths2degree f
  alteration (SIC f) = (f + 1) `div` 7

instance AdditiveGroup SIC where
  zeroV = SIC 0
  negateV (SIC f) = SIC (-f)
  (SIC f1) ^+^ (SIC f2) = SIC $ f1 + f2
  (SIC f1) ^-^ (SIC f2) = SIC $ f1 - f2

instance VectorSpace SIC where
  type Scalar SIC = Int
  s *^ (SIC f) = SIC $ f * s

instance Interval SIC where
  type ICOf SIC = SIC
  ic     = id
  octave = zeroV
  direction (SIC 0) = EQ
  direction i | d == 0    = EQ
              | d < 4     = GT
              | otherwise = LT
    where d = diasteps i

instance Diatonic SIC where
  isStep ic = d == 0 || d == 1 || d == 6 where d = degree ic

instance Chromatic SIC where
  chromaticSemitone = sic 7

instance IntervalClass SIC where
  type IOf SIC = SInterval
  -- ic (SInterval d c) = sic d c
  emb (SIC f) = SInterval f (negate $ f * 4 `div` 7)

instance ToMidi SIC where
  toMidi (SIC f) = f * 7 `mod` 12

instance Notation SIC where
  showNotation i = qual <> show (dia + 1)
   where
    dia  = diasteps i
    alt  = alteration i
    qual = if isPerfect dia
      then qualpf alt 'a' 'P' 'd'
      else qualimpf alt 'a' 'M' 'm' 'd'
  parseNotation = do
    sign <- R.option '+' (R.char '-')
    i    <- sic <$> parseDia
    pure $ if sign == '-' then negateV i else i

-- spelled pitch / pitch class
------------------------------

instance (Spelled i, Interval i, Spelled (ICOf i)) => Spelled (Pitch i) where
  fifths (Pitch i) = fifths i
  octaves (Pitch i) = octaves i
  internalOctaves (Pitch i) = internalOctaves i
  degree (Pitch i) = degree i
  generic (Pitch i) = generic i
  diasteps (Pitch i) = diasteps i
  alteration (Pitch i) = alteration $ ic i

letter :: Spelled i => i -> Char
letter i = chr $ ord 'A' + ((degree i + 2) `mod` 7)

type SPitch = Pitch SInterval
type SPC = Pitch SIC

spelledp :: Int -> Int -> SPitch
spelledp f o = Pitch $ SInterval f o

-- helper
parseAccs :: R.ReadP Int
parseAccs = R.option 0 $ sharps <|> flats
 where
  sharps = length <$> (munchChar1 '♯' <|> munchChar1 '#')
  flats  = negate . length <$> (munchChar1 '♭' <|> munchChar1 'b')

parseName :: R.ReadP Int
parseName = do
  name <- anyChar "ABCDEFG"
  let dia = (ord name - ord 'A' - 2) `mod` 7
  alt <- parseAccs
  return $ ((dia * 2 + 1) `mod` 7) - 1 + 7 * alt

newtype Accidental = Acc Int

flt = Acc (-1)
shp = Acc 1
nat = Acc 0

toSpelled :: Int -> Int -> Accidental -> Int -> SPitch
toSpelled f o (Acc acc) oct =
  spelledp f (o + oct) +^ (chromaticSemitone ^* acc)

c = toSpelled 0 0
d = toSpelled 2 (-1)
e = toSpelled 4 (-2)
f = toSpelled (-1) 1
g = toSpelled 1 0
a = toSpelled 3 (-1)
b = toSpelled 5 (-2)

instance Show SPitch where
  show = showNotation

instance Notation SPitch where
  showNotation p = letter p : accs <> show (octaves p)
    where accs = accstr (alteration p) '♯' '♭'
  parseNotation = do
    f <- parseName
    o <- parseInt
    pure $ spelledp f (o - (f * 4 `div` 7))

instance ToMidi SPitch where
  toMidi (Pitch i) = toMidi i + 12

spc :: Int -> SPC
spc f = Pitch $ sic f

toSPC :: Int -> Accidental -> SPC
toSPC f (Acc acc) = spc f +^ (chromaticSemitone ^* acc)

c' = toSPC 0
d' = toSPC 2
e' = toSPC 4
f' = toSPC (-1)
g' = toSPC 1
a' = toSPC 3
b' = toSPC 5

instance Show SPC where
  show = showNotation

instance Notation SPC where
  showNotation p = letter p : accstr (alteration p) '♯' '♭'
  parseNotation = spc <$> parseName

instance ToMidi SPC where
  toMidi (Pitch ic) = toMidi ic + 60
