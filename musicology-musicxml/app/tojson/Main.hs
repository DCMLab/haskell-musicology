{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Data.Aeson
import qualified Data.Aeson.Encoding           as E
import qualified Data.Aeson.Encode.Pretty      as P
import qualified Data.ByteString.Lazy          as B
import           Options.Applicative

import           Musicology.MusicXML
import           Musicology.Types

data Opts = Opts
  { inFile :: Maybe String
  , outFile :: Maybe String
  , unfold :: Bool
  , pretty :: Bool
  }

optParser :: Parser Opts
optParser =
  Opts
    <$> (argument (Just <$> str) (metavar "INFILE") <|> pure Nothing)
    <*> (argument (Just <$> str) (metavar "OUTFILE") <|> pure Nothing)
    <*> switch (long "unfold" <> short 'u' <> help "Unfold repetitions")
    <*> switch (long "pretty" <> short 'p' <> help "Pretty-print JSON")

noteToJSON
  :: (Notation (Pitch p), ToJSON p, ToJSON t, ToJSON i) => NoteId p t i -> Value
noteToJSON (NoteId p on off id) = object
  ["pitch" .= showNotationT p, "onset" .= on, "offset" .= off, "id" .= id]

main = do
  let getOpts   = info (optParser <**> helper) fullDesc
      prettyCfg = P.defConfig { P.confIndent = P.Spaces 1 }
  opts <- execParser getOpts
  xml  <- case inFile opts of
    Just fn -> B.readFile fn
    Nothing -> B.getContents
  let
    notes  = if unfold opts then xmlNotesHeard xml else xmlNotesWritten xml
    jnotes = noteToJSON <$> asNoteWithId <$> notes
    str =
      if pretty opts then P.encodePretty' prettyCfg jnotes else encode jnotes
  case outFile opts of
    Just fn -> B.writeFile fn str
    Nothing -> B.putStr str
