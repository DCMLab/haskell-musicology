import Control.DeepSeq
import qualified Test.BenchPress as BP
import qualified Criterion.Main as Crit

-- import Musicology.Types
import Musicology.IO.MidiFile
import Musicology.Grams
import Musicology.Polygrams
-- import Musicology.IO.Plotting

-- import qualified StreamingTest

import System.Random.MWC (createSystemRandom)

import Data.Aeson
-- import qualified Data.ByteString.Lazy.Char8 as BS

piecePfx = "/home/chfin/Uni/phd/data/csapp/mozart-piano-sonatas/midi-norep/sonata"
sonata x = piecePfx ++ x ++ ".mid"
pieceFn = sonata "15-1"
plotFn = "/tmp/plot.json"

main :: IO ()
main = mainCrit

testVerticals fn = do
  gen <- createSystemRandom
  notes <- (fmap asWholes) <$> midiLoadNotes fn
  barlen <- pieceBarlen fn
  verticalsR notes 1.0 gen 3 barlen

mainCrit = do
  gen <- createSystemRandom
  barlen <- pieceBarlen pieceFn
  notes <- (fmap asWholes) <$> midiLoadNotes pieceFn
  let notes' = take 500 notes
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
    Crit.bgroup "verticals" [
        Crit.bench "verts (mach)" $ Crit.nf (verticals notes 2) barlen,
        Crit.bench "verts (streamly)" $ Crit.nf (verticals'' 2 barlen) notes,
        Crit.bench "verts (streaming)" $ Crit.nf (verticals''' 2 barlen) notes
        ],
    Crit.bgroup "polygrams" [
        Crit.bench "2x2 (machines)" $ Crit.nf (length . polygrams' 2 2 barlen) notes',
        Crit.bench "2x2 (machines intermediate lists)"
          $ Crit.nf (length . polygrams 2 2 barlen) notes',
        Crit.bench "2x2 (streamly)" $ Crit.nf (length . polygrams'' 2 2 barlen) notes',
        Crit.bench "2x2 (streaming)" $ Crit.nf (length . polygrams''' 2 2 barlen) notes'
        ]
    ]
