{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Musicology.MusicXML where

import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)
import Frames
import Frames.InCore (VectorFor)
import Data.Vinyl.Core (Rec(..))
import qualified Data.Vector as V

-- import Data.ByteString
import Data.List (find, sortOn)
import Data.Maybe (isJust)
import Data.Ratio
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Monad (mapM_)
import Text.Read (readMaybe)

import Musicology.Types

-- import qualified Debug.Trace as D

-- helpers
----------

hasAttrib :: Element -> QName -> Bool
hasAttrib el k = any ((==k) . attrKey) (elAttribs el)

setAttrib :: Element -> QName -> String -> Element
setAttrib el k v = el { elAttribs = attr' }
  where attr' = (Attr k v) : filter ((/=k) . attrKey) (elAttribs el)

ename :: Element -> String
ename = qName . elName

firstInt :: Element -> String -> Maybe Int
firstInt elt subname = do
  sub <- findChild (unqual subname) elt
  readMaybe (strContent sub)

firstInt' :: Element -> String -> Int -> Int
firstInt' elt subname def = maybe def id $ firstInt elt subname

hasChild :: Element -> String -> Bool
hasChild elt name = isJust $ findChild (unqual name) elt

firstChild :: Element -> String -> Maybe Element
firstChild elt name = findChild (unqual name) elt

namedChildren :: Element -> String -> [Element]
namedChildren elt name = findChildren (unqual name) elt

attrIs :: Element -> String -> String -> Bool
attrIs elt name val = maybe False (==val) $ findAttr (unqual name) elt

-- generating ids
-----------------

parseWithIds :: XmlSource s => Bool -> s -> Maybe Element
parseWithIds keep input = do
  root <- parseXMLDoc input
  pure $ fst $ runState (addIds root) 0
  where next :: State Int Int
        next = get >>= \n -> put (n+1) >> pure n
        qID = QName "id" Nothing (Just "xml")
        addIds elt = do
          e <- elt'
          c <- cont'
          pure $ e { elContent = c }
          where elt' = if qName (elName elt) == "note" && (not keep || not (hasAttrib elt qID))
                       then next >>= \i -> pure $ setAttrib elt qID ("note" <> show i)
                       else pure elt
                cont' = mapM contentIds (elContent elt)
        contentIds (Elem e) = Elem <$> addIds e
        contentIds t@(Text _) = pure t
        contentIds c@(CRef _) = pure c

idfy :: Bool -> String -> String
idfy keep input = case parseWithIds keep input of
                    Nothing -> ""
                    Just elt -> showTopElement elt

-- note list
------------

data XmlNote = XmlNote
  { _onset :: Ratio Int
  , _offset :: Ratio Int
  , _dia :: Int
  , _chrom :: Int
  , _part :: Int
  , _id :: Maybe String
  }
  deriving (Eq, Ord, Show)

type instance VectorFor (Ratio Int) = V.Vector
type instance VectorFor (Maybe String) = V.Vector

type XmlRecord = Record '[ "onset" :-> Ratio Int, "offset" :-> Ratio Int
                         , "dia" :-> Int, "chrom" :-> Int
                         , "part" :-> Int, "id" :-> Maybe String
                         ]

xmlNoteToRecord :: XmlNote -> XmlRecord
xmlNoteToRecord (XmlNote on off dia chrom part id) =
  on &: off &: dia &: chrom &: part &: id &: RNil

notesToFrame :: [XmlNote] -> Frame XmlRecord
notesToFrame notes = toFrame $ map xmlNoteToRecord notes

asNote :: XmlNote -> Note SInterval (Ratio Int)
asNote (XmlNote on off dia chrom _ _) = Note (spelled dia chrom) on off

asNoteId :: XmlNote -> NoteId SInterval (Ratio Int) (Maybe String)
asNoteId (XmlNote on off dia chrom _ id) = NoteId (spelled dia chrom) on off id 

data ParsingState = PS
  { psNotes :: [XmlNote]
  -- , psFirstBar :: Bool
  , psDiv :: Int
  , psTime :: Ratio Int
  , psPrevTime :: Ratio Int
  , psTransDia :: Int
  , psTransChrom :: Int
  , psTied :: [XmlNote]
  } deriving Show

xmlNotes :: XmlSource s => s -> [XmlNote]
xmlNotes input = maybe [] scoreNotes (parseXMLDoc input)

scoreNotes :: Element -> [XmlNote]
scoreNotes root = sortOn _onset $ reverse notes
  where notes = if ename root == "score-partwise"
                then concatMap (uncurry partNotes) $ zip (namedChildren root "part") [1..]
                else []

partNotes :: Element -> Int -> [XmlNote]
partNotes part parti = psNotes $ snd $ runState doPart init
  where init = PS [] 1 (0%1) (0%1) 0 0 []
        doPart = do
          mapM_ doMeasure $ namedChildren part "measure"
          modify $ \st -> st { psNotes = psNotes st <> psTied st }
        doMeasure m = mapM_ mElt $ elChildren m
        mElt elt = case ename elt of
          "attributes" -> doAttribs elt
          "note"       -> doNote elt parti
          "forward"    -> doForward elt
          "backup"     -> doBackup elt
          _            -> pure ()

doAttribs :: Element -> State ParsingState ()
doAttribs elt = forM_ (elChildren elt) $ \att -> do
  case ename att of
    "divisions" -> forM_ (readMaybe $ strContent att) $
                   \div -> modify $ \st -> st { psDiv = div }
    "transpose" -> let chrom = firstInt att "chromatic"
                       dia   = firstInt' att "diatonic" 0
                       octs  = firstInt' att "octave-change" 0
                   in case chrom of
                        Nothing -> pure ()
                        Just c  -> modify $
                          \st -> st { psTransChrom = c + 12 * octs
                                    , psTransDia   = dia   + 7  * octs}
    _           -> pure ()
                       
noteNames :: String -> Maybe (Int, Int)
noteNames "C" = Just (0,0)
noteNames "D" = Just (1,2)
noteNames "E" = Just (2,4)
noteNames "F" = Just (3,5)
noteNames "G" = Just (4,7)
noteNames "A" = Just (5,9)
noteNames "B" = Just (6,11)
noteNames _   = Nothing

doNote :: Element -> Int -> State ParsingState ()
doNote note parti = do
  st <- get
  -- time
  let duration = firstInt' note "duration" 0 % (psDiv st * 4)
      isChord = hasChild note "chord"
      onset = if isChord then psPrevTime st else psTime st
      offset = onset + duration
  unless isChord $ put (st { psPrevTime = psTime st })
  modify $ \st -> st { psTime = offset }
  -- st' <- get
  -- D.traceM $ "time: " <> show isChord <> "\t" <> show (psPrevTime st) <> ", " <> show (psTime st)
  --   <> "; " <> show (psPrevTime st') <> ", " <> show (psTime st')

  when (hasChild note "pitch" && not (hasChild note "rest")) $ maybe (pure ()) (>> pure ()) $ do
    -- switch to Maybe monad here
    -- pitch
    pitch <- firstChild note "pitch"
    oct <- firstInt pitch "octave"
    let alt = firstInt' pitch "alter" 0

    step <- firstChild pitch "step"
    (dia', chrom') <- noteNames $ strContent step
    let dia   = psTransDia   st + dia'   + oct * 7
        chrom = psTransChrom st + chrom' + oct * 12 + alt

    -- id
    let id = findAttr (unqual "id") note <|>
             findAttr (QName "id" Nothing (Just "xml")) note

    -- ties
    let ties = namedChildren note "tie"
        anyTieOfType val = any (\tie -> attrIs tie "type" val) ties

    -- tie stop?
    let (tied, onset', id') = if anyTieOfType "stop"
          then let continued (XmlNote _ toff tdia tchrom _ _) =
                     toff == onset && tdia == dia && tchrom == chrom
                   start = find continued (psTied st) in
                 case start of
                   Nothing -> (psTied st, onset, id)
                   Just n@(XmlNote ton _ _ _ _ tid) ->
                     ( filter (/=n) (psTied st)
                     , ton
                     , tid <|> id )
          else (psTied st, onset, id)

    -- tie start?
    let newNote = XmlNote onset' offset dia chrom parti id'
    if (anyTieOfType "start")
      then pure $ modify $ \st -> st { psTied = newNote : tied }
      else pure $ modify $ \st -> st { psNotes = newNote : psNotes st, psTied = tied }
    -- let newNote = XmlNote onset offset dia chrom parti id
    -- pure $ modify $ \st -> st { psNotes = newNote : psNotes st }

doForward :: Element -> State ParsingState ()
doForward elt = do
  st <- get
  let div = psDiv st
      t   = psTime st
      t'  = t + firstInt' elt "duration" 0 % (div * 4)
  put st { psTime = t', psPrevTime = t' }

doBackup :: Element -> State ParsingState ()
doBackup elt = do
  st <- get
  let div = psDiv st
      t   = psTime st
      t'  = t - firstInt' elt "duration" 0 % (div * 4)
  put st { psTime = t', psPrevTime = t' }
