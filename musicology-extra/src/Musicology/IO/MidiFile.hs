{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Musicology.IO.MidiFile where

import Musicology.Core hiding (Pitch)
import Musicology.Internal.Helpers
import Codec.Midi hiding (channel, Channel, Velocity)

import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio
import qualified Data.HashMap.Strict as M
import Data.List (mapAccumL, sortOn)
import Data.Ord (comparing)

import Frames hiding ((:&))
import Frames.InCore (VectorFor)
import Frames.TH (declareColumn)
import Data.Vinyl.Core (Rec(..))
import qualified Data.Vector as V
-- import Pipes

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import System.FilePath (takeExtension, takeBaseName)
import System.Directory (listDirectory)

-- import qualified Data.Machine as MC

data MidiNote = MidiNote
  { _onsetTicks :: Int
  , _offsetTicks :: Int
  , _onsetWholes :: Ratio Int
  , _offsetWholes :: Ratio Int
  , _onsetSecs :: Double
  , _offsetSecs :: Double
  , _pitch :: MidiPitch
  , _velocity :: Int
  , _trackNum :: Int
  , _channel :: Int
  , _keySharps :: Int
  , _keyMajor :: Bool
  , _bar :: Int
  , _beat :: Int
  , _subbeat :: Ratio Int
  }
  deriving (Eq, Show, Generic)

asTicks :: MidiNote -> Note MidiInterval Int
asTicks (MidiNote on off _ _ _ _ pitch _ _ _ _ _ _ _ _) = Note pitch on off

asWholes :: MidiNote -> Note MidiInterval (Ratio Int)
asWholes (MidiNote _ _ on off _ _ pitch _ _ _ _ _ _ _ _) = Note pitch on off

asSecs :: MidiNote -> Note MidiInterval Double
asSecs (MidiNote _ _ _ _ on off pitch _ _ _ _ _ _ _ _) = Note pitch on off

instance NFData MidiNote

type instance VectorFor (Ratio Int) = V.Vector

type MidiRecord = Record '[ "onsetTicks" :-> Int, "offsetTicks" :-> Int
                          , "onsetWholes" :-> Ratio Int, "offsetWholes" :-> Ratio Int
                          , "onsetSecs" :-> Double, "offsetSecs" :-> Double
                          , "pitch" :-> Int, "velocity" :-> Int, "trackNum" :-> Int
                          , "channel" :-> Int, "keySharps" :-> Int, "keyMajor" :-> Bool
                          , "bar" :-> Int, "beat" :-> Int, "subbeat" :-> Ratio Int
                          ]
type RatioInt = Ratio Int

declareColumn "onsetTicks" ''Int
declareColumn "offsetTicks" ''Int
declareColumn "onsetWholes" ''RatioInt
declareColumn "offsetWholes" ''RatioInt
declareColumn "onsetSecs" ''Int
declareColumn "offsetSecs" ''Int
declareColumn "pitch" ''Int
declareColumn "velocity" ''Int
declareColumn "trackNum" ''Int
declareColumn "channel" ''Int
declareColumn "keySharps" ''Int
declareColumn "keyMajor" ''Bool
declareColumn "bar" ''Int
declareColumn "beat" ''Int
declareColumn "subbeat" ''RatioInt

midiNoteToRecord :: MidiNote -> MidiRecord
midiNoteToRecord (MidiNote ont oft onw ofw ons ofs p v t c s m bar beat sub) =
  ont &: oft &: onw &: ofw &: ons &: ofs &:
  toInterval p &: v &: t &: c &: s &: m &: bar &: beat &: sub &: RNil

printNote :: MidiNote -> IO ()
printNote (MidiNote ont offt onw offw ons offs p vel id ch sh maj bar beat subb) =
  putStrLn $ show ont ++ "\t" ++ show offt ++ "\t" ++
  show onw ++ "\t" ++ show offw ++ "\t" ++
  show ons ++ "\t" ++ show offs ++ "\t" ++
  show p ++ "\t" ++ show vel ++ "\t" ++ show id ++ "\t" ++
--  show ch ++ "\t" ++ show sh ++ "\t" ++ show maj ++ "\t" ++
  show bar ++ "\t" ++ show beat ++ "\t" ++ show subb

type PrepEv a = (Int, Message, (a, Message))

prepareTrack :: Track a -> Int -> [PrepEv a]
prepareTrack trk trkNum = catMaybes $ (snd $ mapAccumL nextEv init trk)
  where init = KeySignature 0 0
        nextEv :: Message -> (a, Message) -> (Message, Maybe (PrepEv a))
        nextEv _ (_, ks@(KeySignature _ _)) = (ks, Nothing)
        nextEv ks ev = (ks, Just (trkNum, ks, ev))

mergeTracks :: Ord a => [PrepEv a] -> [PrepEv a] -> [PrepEv a]
mergeTracks = mergeBy $ comparing getTime
  where getTime (_, _, (time, _)) = time

midiEvents :: Midi -> [PrepEv Ticks]
midiEvents midi = foldl mergeTracks [] prepared
  where prep t = prepareTrack (toAbsTime t)
        prepared = zipWith prep (tracks midi) [0..]

-- produceEvents :: Monad m => Midi -> Producer (PrepEv Ticks) m ()
-- produceEvents = each . midiEvents

timeRatios :: TimeDiv -> Int -> (Ratio Int, Double)
timeRatios (TicksPerBeat ppq) mspq =
  (1 % (4*ppq), fromIntegral mspq / (1000000.0 * fromIntegral ppq))
timeRatios (TicksPerSecond tpf fps) mspq =
  (250000 % (tps * mspq), 1.0 / fromIntegral mspq)
  where tps = tpf * fps

type Resolver a = [a] -> [a] -> [a]

queue :: Resolver a
queue ls x = ls ++ x

stack :: Resolver a
stack ls x = x ++ ls

data OnVal = OnVal
  { ovNowT   :: Int
  , ovNowW   :: (Ratio Int)
  , ovNowS   :: Double
  , ovVel    :: Int
  , ovKeySig :: Message
  , ovBar    :: Int
  , ovBeat   :: Int
  , ovSubbeat :: Ratio Int
  }
type Ons = M.HashMap (Int, Int, Int) [OnVal]
type Coeffs a = (a, a)
data MidiState = MidiState
  { msOns     :: Ons
  , msWCoeffs :: Coeffs (Ratio Int)
  , msSCoeffs :: Coeffs Double
  , msBarOff  :: Ratio Int
  , msBarRef  :: Int
  , msBarLen  :: Ratio Int
  , msBeatLen :: Ratio Int
  }

eventsToNotes :: [PrepEv Ticks] -> TimeDiv -> Resolver OnVal -> [MidiNote]
eventsToNotes evs tdiv insNote = sortOn sorting notes 
  where notes = catMaybes $ snd $ mapAccumL nextEv init evs
        sorting note = (_onsetTicks note, _trackNum note, _channel note)
        toTime x coeffs = fromIntegral x * (snd coeffs) + (fst coeffs)
        newCoeffs tks now ratio = (now - ratio * fromIntegral tks, ratio)
        initRatios = timeRatios tdiv 500000
        init = MidiState M.empty (0%1, fst initRatios) (0.0, snd initRatios) (0%1) 0 (1%1) (1%4)

        nextEv :: MidiState
               -> PrepEv Ticks
               -> (MidiState, Maybe MidiNote)
        nextEv
          st@(MidiState ons wcoeffs scoeffs baroff barref barlen beatlen)
          (trackid, keysig, (nowt, msg))
          = processMsg msg
          where noww = toTime nowt wcoeffs
                nows = toTime nowt scoeffs
                relbar = (noww - baroff) / barlen
                (rawbar, inbar) = properFraction relbar
                nowbar = barref + rawbar
                (nowbeat, nowsubb) = properFraction $ inbar / beatlen
                
                processMsg :: Message -> (MidiState, Maybe MidiNote)

                processMsg (TempoChange tempo) = (st', Nothing)
                  where ratios = timeRatios tdiv tempo
                        st' = st { msWCoeffs = newCoeffs nowt noww (fst ratios)
                                 , msSCoeffs = newCoeffs nowt nows (snd ratios) }

                processMsg (TimeSignature num den _ _) = (st', Nothing)
                  where ref'  = if inbar == 0 then nowbar else nowbar + 1
                        denom = 2^den -- by MIDI standard, denom is encoded as exponent of 2
                        st'   = st { msBarOff  = noww
                                   , msBarRef  = ref'
                                   , msBarLen  = num % denom
                                   , msBeatLen = 1 % denom }

                processMsg (NoteOn ch p 0) = processMsg $ NoteOff ch p 0

                processMsg (NoteOn ch p vel) = (st { msOns = ons' }, Nothing)
                  where nKey = (trackid, ch, p)
                        nVal = OnVal nowt noww nows vel keysig nowbar nowbeat nowsubb
                        ons' = M.insertWith insNote nKey [nVal] ons

                processMsg (NoteOff ch p vel) = (st { msOns = ons' }, note)
                  where nKey = (trackid, ch, p)
                        open = M.lookupDefault [] nKey ons
                        note = case open of
                                 [] -> Nothing
                                 (OnVal ont onw ons onvel
                                  (KeySignature sh mode) bar beat subb):_ ->
                                   Just $ MidiNote ont nowt onw noww ons nows
                                   (toPitch p) onvel trackid ch sh (mode == 0) bar beat subb
                        ons' = case open of
                                 []    -> ons
                                 _:rst -> M.insert nKey rst ons

                processMsg _ = (st, Nothing)

-- processNotes :: Monad m => TimeDiv -> Resolver OnVal -> Pipe (PrepEv Ticks) MidiNote m ()
-- processNotes = undefined

notesToFrame :: [MidiNote] -> Frame MidiRecord
notesToFrame notes = toFrame $ map midiNoteToRecord notes

midiTracks :: FilePath -> IO [Track Ticks]
midiTracks fp = do
  midi <- importFile fp
  case midi of
    Left _ -> return []
    Right m -> return $ tracks m

midiLoadEvs fp =
  importFile fp >>= return . either (const []) midiEvents

midiLoadNotes fp =
  importFile fp >>= return . either (const []) loadNotes
  where loadNotes m =
          eventsToNotes (midiEvents m) (timeDiv m) queue

pieceBarlen fp =
  importFile fp >>= return . either (const $ 1%1) barlen
  where barlen m = maybe (1%1) id (listToMaybe (tracks m) >>= findTime)
          where findTime ((_,(TimeSignature num den _ _)):_) = Just $ num % (2^den)
                findTime ((_,ev):rst) = findTime rst
                findTime _            = Nothing

pieceBeatlen fp =
  importFile fp >>= return . either (const $ 1%4) barlen
  where barlen m = maybe (1%4) id (listToMaybe (tracks m) >>= findTime)
          where findTime ((_,(TimeSignature _ den _ _)):_) = Just $ 1 % (2^den)
                findTime ((_,ev):rst) = findTime rst
                findTime _            = Nothing

dirMidiPieces :: FilePath -> IO [FilePath]
dirMidiPieces dir = do
  contents <- listDirectory dir
  return $ takeBaseName <$> filter ((==".mid") . takeExtension) contents
