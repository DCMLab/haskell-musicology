{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module: Musicology.Pitch.Class
Description: Generic pitch and interval interface.
Copyright: Christoph Finkensiep, 2021
License: BSD
Maintainer: chfin@chfin.de
Stability: experimental

This module defines a generic interface for pitch and interval types.

It provides three basic blocks of functionality:

- a set of type classes for generically [working with intervals](#g:1)
- a generic type and associated operations for [working with pitches](#g:2)
- functionality for reading and printing pitches and intervals in a [common notation](#g:3)
-}
module Musicology.Pitch.Class
  (
  -- * Intervals
  --
  -- | Every interval implements the 'Interval' class.
  -- Since intervals form vector spaces (or rather [modules]()),
  -- 'Interval' inherits from 'VectorSpace' and the associated arithmetic operations '^+^', '^-^', '*^', and '^*'.
  -- Each 'Interval' type is associated with an 'IntervalClass' type that implements octave equivalence.
  -- In addition, the classes 'Diatonic' and 'Chromatic' provide extra functionality
  -- for intervals that have diatonic and chromatic interpretations.
  module Data.VectorSpace
  , Interval(..)
  , IntervalClass(..)
  , oct
  , unison
  -- , ClassyInterval(..)
  , Diatonic(..)
  , Chromatic(..)
  , aug
  , dim
  , down
  , ImperfectInterval(..)
  , minor
  , major
  -- * Pitches
  --
  -- | Pitches are derived from intervals by interpreting an interval relative to a conventional reference point,
  -- which is specific to the interval type at hand.
  -- A pitch is represented as a 'Pitch' value, which is a newtype wrapper around an interval type.
  -- Calculating with pitches and intervals is done with
  --
  -- - 'pto', 'pfrom' between pitches,
  -- - '+^', '^+', '-^' between pitches and intervals (the @^@ is on the interval side)
  -- - 'pc' for turning a pitch into a pitch class.
  , Pitch(..)
  , toPitch
  , toInterval
  , pto
  , pfrom
  , (+^)
  , (^+)
  , (-^)
  , pc
  -- * Notation
  --
  -- | The 'Notation' class implements showing and reading a standard notation
  -- that is compatible with other implementations of this library (for standard interval and pitch types).
  , Notation(..)
  -- * Other Functions
  , transpose
  , embedI
  , embedP
  , embed
  , embed'
  -- * Conversion Classes
  , ToMidi(..)
  , ToFreq(..)
  )
where

import           Data.VectorSpace

import           GHC.Generics                   ( Generic )
import qualified Data.Text                     as T
import qualified Text.ParserCombinators.ReadP  as R
import           Control.DeepSeq                ( NFData )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Maybe                     ( listToMaybe )
import           Data.Hashable                  ( Hashable )

---------------
-- Intervals --
---------------


class (Interval i, Interval (IOf i), VectorSpace i, ICOf (IOf i) ~ i) => IntervalClass i where
  type IOf i
--  ic :: IOf i -> i
  emb :: i -> IOf i

class VectorSpace i => Interval i where
  type ICOf i
  ic :: i -> ICOf i
  octave :: i
  direction :: i -> Ordering
  default direction :: Ord i => i -> Ordering
  direction i = compare i zeroV
  iabs :: i -> i
  iabs i | direction i == LT = negateV i
         | otherwise         = i

oct :: (IntervalClass i, s ~ Scalar (IOf i)) => s -> i -> IOf i
oct octs ic = emb ic ^+^ (octave ^* octs)

unison :: Interval i => i
unison = zeroV

-- class (Interval i, IntervalClass (PCOf i), IOf (ICOf i) ~ i) => ClassyInterval i where  

class Interval i => Diatonic i where
  isStep :: i -> Bool

class Interval i => Chromatic i where
  chromaticSemitone :: i

down :: Interval i => i -> i
down = negateV

newtype ImperfectInterval i = Impf (i -> i)

minor :: Chromatic i => ImperfectInterval i -> i
minor (Impf int) = int chromaticSemitone

major :: Interval i => ImperfectInterval i -> i
major (Impf int) = int zeroV

dim :: Chromatic i => i -> i
dim = (^-^ chromaticSemitone)

aug :: Chromatic i => i -> i
aug = (^+^ chromaticSemitone)

class Notation i where
  showNotation  :: i -> String
  showNotationT :: i -> T.Text
  showNotationT = T.pack . showNotation
  parseNotation :: R.ReadP i
  readNotation  :: String -> Maybe i
  readNotation str = fst <$> listToMaybe (R.readP_to_S parseFull str)
    where parseFull = do
            result <- parseNotation
            R.eof
            pure result
  readNotationT :: T.Text -> Maybe i
  readNotationT = readNotation . T.unpack

-------------
-- Pitches --
-------------

-- wrapper type: turn intervals into pitches
newtype Pitch a = Pitch a
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, NFData, Hashable)

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

-------------
-- Helpers --
-------------

transpose :: (Functor f, Interval i) => i -> f i -> f i
transpose by = fmap (^+^ by)

embedI rot trans = (^+^ trans) . emb . (^+^ rot)

embedP rot trans = (+^ trans) . fmap emb . (+^ rot)

embed rot trans = transpose trans . fmap emb . transpose rot
embed' slide c0 = embed (negateV (ic slide)) (toInterval c0 ^+^ slide)

----------------
-- conversion --
----------------

class ToMidi i where
  toMidi :: i -> Int

class ToFreq i where
  toFreq :: i -> Double
