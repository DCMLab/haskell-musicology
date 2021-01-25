{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Data.Aeson
import qualified Data.Aeson.Encoding           as E
import qualified Data.Aeson.Encode.Pretty      as P
import qualified Data.ByteString.Lazy          as B

import           Options.Applicative

import           Musicology.MusicXML
import           Musicology.Core
import           Data.Ratio

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

noteToJSON :: (Notation (Pitch p), ToJSON i) => NoteId p (Ratio Int) i -> Value
noteToJSON (NoteId p on off noteid) = object
  ["p" .= showNotationT p, "on" .= lower on, "off" .= lower off, "id" .= noteid]
  where lower r = object ["n" .= numerator r, "d" .= denominator r]

main :: IO ()
main = do
  let getOpts   = info (optParser <**> helper) fullDesc
      prettyCfg = P.defConfig { P.confIndent = P.Spaces 1 }
  opts <- execParser getOpts
  xml  <- maybe B.getContents B.readFile $ inFile opts
  let
    notes  = if unfold opts then xmlNotesHeard xml else xmlNotesWritten xml
    jnotes = noteToJSON . asNoteWithId <$> notes
    str =
      if pretty opts then P.encodePretty' prettyCfg jnotes else encode jnotes
  case outFile opts of
    Just fn -> B.writeFile fn str
    Nothing -> B.putStr str
