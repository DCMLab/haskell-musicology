{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module CMaj where

import           Musicology.Types
import           Musicology.MusicXML
import qualified Data.ByteString               as B
import qualified Data.Map.Strict               as M
import           Data.Ratio
import qualified Data.List                     as L
import           Data.Functor.Identity

prelude = B.readFile "app/prelude.xml" >>= pure . xmlNotesWritten

counts :: (Foldable t) => M.Map k (t v) -> M.Map k Int
counts = M.map length

splitHands = L.partition (\n -> pitch n < c nat 4)

-- preamble = "\\version \"2.19.82\"\
--   \\\paper {\
--   \  #(set-paper-size \"a4\" 'landscape)\
--   \  top-margin: \
--   \}\
--   \\\header {\
--   \  tagline = ##f \
--   \}"

score fstaff fwrap notes =
  "\\score {\n\\new PianoStaff {<<\n"
    <> treble
    <> bass
    <> "\n>> \\bar \"|.\"}\n}"
 where
  (notesl, notesr) = splitHands notes
  treble           = fwrap $ "\\clef treble\n" <> fstaff notesr
  bass             = fwrap $ "\\clef bass\n" <> fstaff notesl


-- in voices
------------

groupByPitch :: [XmlNote] -> M.Map SPitch [Ratio Int]
groupByPitch notes = foldr addNote M.empty notes
 where
  addNote note groups =
    M.insertWith (<>) (pitch note) [offset note - onset note] groups

voices :: M.Map SPitch [Ratio Int] -> M.Map SPitch B.ByteString
voices groups = M.mapWithKey lilyfy sorted
 where
  sorted = M.map (L.sortOn id) groups
  lilyfy p ds = B.intercalate " \\bar \"\" " $ fmap (lilynote p) ds

lilynote p d = B.intercalate " ~ \\bar \"\" " $ (lilypitch p <>) <$> lilydur d

dianames = ["c", "d", "e", "f", "g", "a", "b"]

alterate :: Int -> B.ByteString -> B.ByteString -> B.ByteString
alterate 0 _ _ = ""
alterate n u d | n > 0     = B.concat $ take n $ repeat u
               | otherwise = B.concat $ take (abs n) $ repeat d

diachrom = [0, 2, 4, 5, 7, 9, 11]
dia2chrom d = (diachrom !! (mod d 7)) + 12 * (div d 7)

diaget lst = (lst !!) . (flip mod 7)

lilypitch :: SPitch -> B.ByteString
lilypitch (Pitch (SInterval d c)) =
  diaget dianames d <> alterate accs "is" "es" <> alterate (div d 7 - 3) "'" ","
  where accs = c - dia2chrom d

lilydur d | d == (1 % 1) = ["1"]
          | d == 1 % 2   = ["2"]
          | d == 1 % 16  = ["8"]
          | d == 7 % 16  = ["8.", "4"]
          | d == 15 % 16 = ["8.", "4", "2"]

staff notes = B.intercalate "\n\\\\\n" $ mkVoice <$> M.elems
  (voices $ groupByPitch notes)
  where mkVoice v = "{ " <> v <> " }"

wrapStaff s = "\\new Staff {\n \\cadenzaOn\n <<\n" <> s <> "\n>>\n}\n"

artwork = score staff wrapStaff
-- in chords

groupByDuration :: [XmlNote] -> M.Map (Ratio Int) (M.Map SPitch Int)
groupByDuration notes = foldr addNote M.empty notes
 where
  addNote note groups = M.insertWith (M.unionWith (+))
                                     (offset note - onset note)
                                     (M.singleton (pitch note) 1)
                                     groups

chords :: M.Map (Ratio Int) (M.Map SPitch Int) -> [B.ByteString]
chords groups = concat $ groupChords <$> reverse (M.assocs groups)
 where
  groupChords (dur, pitches) = L.unfoldr (nextChord dur) pitches
  nextChord dur pitches
    | M.null pitches = Nothing
    | otherwise      = Just (chord dur $ M.keys pitches, pitches')
    where pitches' = takeChord pitches
  takeChord pitches = runIdentity $ M.traverseMaybeWithKey decOrDel pitches
   where
    decOrDel _ v = if v <= 1 then Identity Nothing else Identity $ Just (v - 1)
  chord dur ps = B.intercalate " ~ " $ (lilychord ps <>) <$> lilydur dur
  lilychord ps = "<" <> B.intercalate " " (lilypitch <$> ps) <> ">"

staff2 notes = B.intercalate " \\bar \"\"\n" $ (chords . groupByDuration) notes

wrapStaff2 s = "\\new Staff {\n \\cadenzaOn\n" <> s <> "\n}\n"

artwork2 = score staff2 wrapStaff2

artwork3 notes =
  "\\score {\n\\new PianoStaff {\n\\cadenzaOn\n\\autochange {"
    <> staff2 notes
    <> "\n}\n}\n}"

chordPairs
  :: M.Map (Ratio Int) (M.Map SPitch Int) -> [(B.ByteString, B.ByteString)]
chordPairs groups =
  L.intercalate [("\\bar \"\" \\break", "\\bar \"\" \\break")]
    $   padGroup
    <$> reorder
    <$> groupChords
    <$> reverse (M.assocs groups)
 where
  groupChords (dur, pitches) = L.unfoldr (nextChord dur) pitches
  nextChord dur pitches
    | M.null pitches = Nothing
    | otherwise      = Just (chordPair dur $ M.keys pitches, pitches')
    where pitches' = takeChord pitches
  takeChord pitches = runIdentity $ M.traverseMaybeWithKey decOrDel pitches
   where
    decOrDel _ v = if v <= 1 then Identity Nothing else Identity $ Just (v - 1)
  chordPair dur ps = (chord dur l, chord dur r)
    where (l, r) = L.partition (< c nat 4) ps
  chord dur ps
    | null ps   = B.intercalate " " $ ("s" <>) <$> lilydur dur
    | otherwise = B.intercalate " ~ " $ (lilychord ps <>) <$> lilydur dur
  lilychord ps = "<" <> B.intercalate " " (lilypitch <$> ps) <> ">"
  reorder lst = reverse evens <> odds
   where
    (e, o) = L.partition (even . fst) $ zip [1 ..] lst
    evens  = map snd e
    odds   = map snd o
  padGroup lst | length lst < 5 = [(hr, hr)] <> pad <> lst <> pad
               | otherwise      = lst
   where
    pad = take 5 $ repeat ("r1", "r1")
    hr  = "\\hide Rest"

artwork4 notes = "\\score {\n\\new PianoStaff {<<" <> treble <> bass <> ">>}}"
 where
  (chordsl, chordsr) =
    unzip
      $ -- reorder $
        chordPairs
      $ groupByDuration notes
  treble = wrapStaff2 $ "\\clef treble\n" <> B.intercalate "\n" chordsr
  bass   = wrapStaff2 $ "\\clef bass\n" <> B.intercalate "\n" chordsl
  reorder lst = reverse evens <> odds
   where
    (e, o) = L.partition (even . fst) $ zip [1 ..] lst
    evens  = map snd e
    odds   = map snd o
                -- lst' = L.sortOn (\(a,b) -> negate $ B.length a + B.length b) lst

main = do
  p <- prelude
  B.writeFile "app/art_content.ily" $ artwork4 p
