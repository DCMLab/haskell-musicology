{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module: Musicology.Pitch
Description: Representations and operations for musical pitch.
Copyright: Christoph Finkensiep, 2021
License: BSD
Maintainer: chfin@chfin.de
Stability: experimental

This module defines a generic interface for pitch and interval types.

It provides three basic blocks of functionality
-}
module Musicology.Pitch.Class
  ( module Data.VectorSpace
  , IntervalClass(..)
  , Interval(..)
  , oct
  , unison
  -- , ClassyInterval(..)
  , Diatonic(..)
  , Chromatic(..)
  , ImperfectInterval(..)
  , aug
  , dim
  , down
  , minor
  , major
  , Pitch(..)
  , toPitch
  , toInterval
  , pto
  , pfrom
  , (+^)
  , (^+)
  , (-^)
  , pc
  , Notation(..)
  , transpose
  , embedI
  , embedP
  , embed
  , embed'
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
