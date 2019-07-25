module Main where

import Musicology.MusicXML
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Musicology.Types

notateNoteId :: Notation p => NoteId p t i -> NoteId String t i
notateNoteId (NoteId (Pitch p) on off id) = NoteId (Pitch (showNotation p)) on off id

main = do
  xml <- getContents
  B.putStr $ encode $ (notateNoteId <$> asNoteWithId <$> xmlNotesHeard xml)
