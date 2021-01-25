import Control.DeepSeq
import qualified Test.BenchPress as BP
import qualified Criterion.Main as Crit

-- import Musicology.Types
import Musicology.IO.MidiFile
import Musicology.Grams
import Musicology.Polygrams
import Musicology.IO.Plotting

import qualified StreamingTest

import System.Random.MWC (createSystemRandom)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

piecePfx = "/home/chfin/Uni/phd/data/csapp/mozart-piano-sonatas/midi-norep/sonata"
sonata x = piecePfx ++ x ++ ".mid"
pieceFn = sonata "15-1"
plotFn = "/tmp/plot.json"

main :: IO ()
main = mainCrit
  -- do
  -- benchSkipgrams
  -- benchSkipgrams'

testVerticals fn = do
  gen <- createSystemRandom
  notes <- (fmap asWholes) <$> midiLoadNotes fn
  barlen <- pieceBarlen fn
  verticalsR notes 1.0 gen 3 barlen

printNotes fn n = do
  notes <- (fmap asSecs) <$> midiLoadNotes fn
  BS.putStrLn $ encode $ take n notes

plotNotes fn n = do
  notes <- (fmap asSecs) <$> midiLoadNotes fn
  ppSpec $ pianoroll $ take n notes

writeNotesPlot fn inds = do
  --gen      <- createSystemRandom
  barlen   <- pieceBarlen fn
  notes    <- (fmap asWholes) <$> midiLoadNotes fn
  let polys = polygrams notes 2 2 barlen
  viewPlot $ plotpolys notes (fmap (polys!!) inds)

mainCrit = do
  gen <- createSystemRandom
  barlen <- pieceBarlen pieceFn
  notes <- (fmap asWholes) <$> midiLoadNotes pieceFn
  Crit.defaultMain [
    Crit.bgroup "midi" [ Crit.bench "loadNotes" $ Crit.nfIO (midiLoadNotes pieceFn) ],
    Crit.bgroup "skipgrams" [
        Crit.bench "indexSkipgrams" $
          Crit.nf (indexSkipgrams ([1..1000] :: [Int]) 3) 20,
        Crit.bench "indexSkipgramsS" $
          Crit.nf (indexSkipgramsS ([1..1000] :: [Int]) 3) 20,
        Crit.bench "indexSkipgramsR" $
          Crit.nfIO $ indexSkipgramsR ([1..1000] :: [Int]) 0.1 gen 3 20,
        Crit.bench "indexSkipgramsRS" $
          Crit.nfIO $ indexSkipgramsRS ([1..1000] :: [Int]) 0.1 gen 3 20
        ],
    Crit.bgroup "skipgrams (streaming)" [
        Crit.bench "indexSkipgrams" $
          Crit.nf (StreamingTest.indexSkipgrams ([1..1000] :: [Int]) 3) 20,
        Crit.bench "indexSkipgramsS" $
          Crit.nf (StreamingTest.indexSkipgramsS ([1..1000] :: [Int]) 3) 20,
        Crit.bench "indexSkipgramsR" $
          Crit.nfIO $ StreamingTest.indexSkipgramsR ([1..1000] :: [Int]) 0.1 gen 3 20,
        Crit.bench "indexSkipgramsRS" $
          Crit.nfIO $ StreamingTest.indexSkipgramsRS ([1..1000] :: [Int]) 0.1 gen 3 20
        ],
    Crit.bgroup "polygrams" [
        Crit.bench "verts3" $ Crit.nf (verticals notes 2) barlen,
        Crit.bench "verts3 (streaming)" $ Crit.nf (StreamingTest.verticals notes 2) barlen
        -- Crit.bench "2x2" $ Crit.nf (polygrams' (take 200 notes) 2 2) barlen
        ]
    ]

benchNotes = mybench "Notes" $ do
  notes <- midiLoadNotes pieceFn
  return $ rnf notes

benchFrameFromNotes = mybench "Frame (via Notes)" $ do
  notes <- midiLoadNotes pieceFn
  let frame = notesToFrame notes
  return $ rnf frame

mybench :: String -> IO () -> IO ()
mybench name action = do
  (cpu, wall) <- BP.benchmark 100 (return ()) (const $ return ()) $ const $ action
  putStrLn $ "\nStarting benchmark " ++ name
  putStrLn "\nCPU Time:"
  BP.printDetailedStats cpu
  putStrLn "\nClock Time:"
  BP.printDetailedStats wall
