{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Musicology.MusicXML
  ( parseWithIds, idfy
  , XmlNote(..), XmlRecord
  , xmlNoteToRecord, notesToFrame
  , asNote, asNoteWithId
  , xmlNotes, xmlNotesWritten, xmlNotesHeard, scoreNotes
  ) where

import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)
import Frames
import Frames.InCore (VectorFor)
import Data.Vinyl.Core (Rec(..))
import qualified Data.Vector as V
import qualified Data.Map as M

-- import Data.ByteString
import Data.Foldable (for_)
import Data.List (sort, find, sortOn, uncons)
import Data.Maybe (isJust, catMaybes, listToMaybe)
import Data.Ratio
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Monad (mapM_)
import Text.Read (readMaybe)
import Lens.Micro

import Musicology.Types

import qualified Debug.Trace as D

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

readIntList :: String -> Maybe [Int]
readIntList str = readMaybe $ "[" <> str <> "]"

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

instance Timed XmlNote where
  type TimeOf XmlNote = Ratio Int

instance HasTime XmlNote where
  onsetL f note = fmap (\on' -> note { _onset = on' }) (f $ _onset note)
  offsetL f note = fmap (\off' -> note { _offset = off' }) (f $ _offset note)

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

asNoteWithId :: XmlNote -> NoteId SInterval (Ratio Int) (Maybe String)
asNoteWithId (XmlNote on off dia chrom _ id) = NoteId (spelled dia chrom) on off id 

-- musical control flow
-----------------------

data FlowMarker t = FM t FlowCommand -- (Maybe [Int])
  deriving (Eq, Ord, Show)

-- order: closing low, closing high, opening high, opening low
data FlowCommand = BwRepeat Int
                 | StopEnding
                 | Fine
                 | DaCapo (Maybe [Int])
                 | DalSegno String (Maybe [Int])
                 | ToCoda String (Maybe [Int])
                 | Coda String
                 | Segno String
                 | StartEnding [Int]
                 | FwRepeat
  deriving (Eq, Ord, Show)

type RepStack t = [(t, Int, [FlowMarker t])]

data FlowState t = FS
  { fsNow :: t -- current time
  , fsMarkers :: [FlowMarker t] -- all markers from here to end of piece
  , fsSegno :: M.Map String ([FlowMarker t], t, (RepStack t)) -- segnos with their repetition stacks
  , fsStack :: RepStack t -- repetition stack
  , fsGCount :: M.Map (FlowMarker t) Int -- how often did we depart from this location?
  , fsJumps :: [Jump t] -- output: jumps
  } deriving (Show)

data Jump t = Jump t t
            | End t
  deriving (Show, Eq, Ord)

nextMarker :: Ord t => State (FlowState t) (Maybe (FlowMarker t))
nextMarker = do
  st <- get
  case uncons $ fsMarkers st of
    Just (m@(FM t _), ms) -> do
      let -- prev = fsNow st
          gc' = M.insertWith (+) m 1 (fsGCount st)
      put (st { fsMarkers = ms, fsNow = t, fsGCount = gc' }) >> pure (Just m)
    Nothing -> pure Nothing

unfoldFlow :: Ratio Int -> [FlowMarker (Ratio Int)] -> [(Ratio Int, Ratio Int)]
unfoldFlow end markers = foldJumps $ fsJumps $ snd $ runState doFlow init
  where foldJumps lst = fj end lst []
        fj t [] acc = (0%1, t) : acc
        fj t (End e:rst) acc = fj e rst acc
        fj t (Jump t1 t2:rst) acc = fj t1 rst ((t2,t) : acc)
        markersSorted = sort markers -- sort respects marker precedence (see above)
        init = FS (0%1) markersSorted M.empty [] M.empty []
        restore markers t stack = do
          modify (\st -> st { fsMarkers = markers, fsNow = t, fsStack = stack
                            , fsJumps = Jump (fsNow st) t : fsJumps st })
        reenter markers t = modify $ \st -> st { fsMarkers = markers, fsNow = t, fsStack = []
                                               , fsJumps = Jump (fsNow st) t : fsJumps st }
        pushRepeat = modify $ \st -> st { fsStack = (fsNow st, 1, fsMarkers st) : fsStack st }
        popRepeat = modify $ \st -> st { fsStack = maybe [] snd $ uncons $ fsStack st }
        goRepeat = do
          stack <- fsStack <$> get
          case (uncons stack) of -- repeat from repeat sign or beginning?
            Just ((t, n, markers), rst) -> restore markers t ((t, n+1, markers) : rst)
            Nothing -> restore markersSorted (0%1) [(0%1, 2, markersSorted)]
        skipUntil com = do
          markers <- fsMarkers <$> get
          let dropped = dropWhile (\(FM _ c) -> c /= com) markers
              (FM t _, rest) = maybe (FM (0%1) com, []) id $ uncons dropped
          modify $ \st -> st { fsMarkers = rest, fsNow = t 
                             , fsJumps = Jump (fsNow st) t : fsJumps st }
        currentRep = maybe 1 (\(_, n, _) -> n) . listToMaybe . fsStack <$> get
        checkTimes Nothing _ action = action
        checkTimes (Just times) marker action = do
          gc <- fsGCount <$> get
          when (passes gc) action
          where passes counts = M.findWithDefault 1 marker counts `elem` times
        doFlow = do
          -- get >>= D.traceM . show
          mm <- nextMarker
          for_ mm $ \(marker@(FM t com)) -> do
            case com of
              FwRepeat       -> pushRepeat
              BwRepeat n     -> do
                rep <- currentRep
                if rep < n then goRepeat else popRepeat
              StartEnding ns -> do
                n <- currentRep
                if n `elem` ns then pure () else skipUntil StopEnding
              StopEnding     -> pure ()
              Fine           -> modify $ \st -> st { fsMarkers = [] -- stops recursion
                                                   , fsJumps = End (fsNow st) : fsJumps st }
              DaCapo times   -> checkTimes times marker $ reenter markersSorted (0%1)
              Segno name     -> modify $ \st ->
                let now = fsNow st
                    stack = fsStack st
                    segnos = M.insert name (markers, now, stack) $ fsSegno st
                in st { fsSegno = segnos }
              DalSegno name t -> checkTimes t marker $ get >>= \st ->
                for_ (M.lookup name $ fsSegno st) $ \(m, t, s) -> restore m t s 
              Coda name      -> pure ()
              ToCoda name t  -> checkTimes t marker $ skipUntil $ Coda name
            doFlow

runFlow :: (HasTime n, TimeOf n ~ t, Num t, Ord t) => [(t,t)] -> [n] -> [n]
runFlow blocks notes = snd $ foldl nextBlock (0, []) blocks
  where nextBlock (now,acc) (t1,t2) = (now', acc')
          where shift = (+(now-t1))
                now'  = shift t2
                nts   = filter (\n -> (offset n > t1) && (onset n < t2)) notes
                acc'  = acc <> fmap ((onsetL %~ shift) . (offsetL %~ shift)) nts

-- parsing to notelist
----------------------

data ParsingState = PS
  { psNotes :: [XmlNote]
  , psDiv :: Int
  , psTime :: Ratio Int
  , psPrevTime :: Ratio Int
  , psTransDia :: Int
  , psTransChrom :: Int
  , psTied :: [XmlNote]
  , psFlow :: [FlowMarker (Ratio Int)]
  } deriving Show

xmlNotes :: XmlSource s => Bool -> s -> [XmlNote]
xmlNotes unfoldReps input = maybe [] (scoreNotes unfoldReps) (parseXMLDoc input)

xmlNotesWritten :: XmlSource s => s -> [XmlNote]
xmlNotesWritten = xmlNotes False

xmlNotesHeard :: XmlSource s => s -> [XmlNote]
xmlNotesHeard = xmlNotes True

scoreNotes :: Bool -> Element -> [XmlNote]
scoreNotes unfoldReps root = sortOn _onset $ reverse notes
  where notes = if ename root == "score-partwise"
                then concatMap (uncurry $ partNotes unfoldReps) $ zip (namedChildren root "part") [1..]
                else []

partNotes :: Bool -> Element -> Int -> [XmlNote]
partNotes unfold part parti = psNotes $ snd $ runState doPart init
  where init = PS [] 1 (0%1) (0%1) 0 0 [] []
        doPart = do
          mapM_ doMeasure $ namedChildren part "measure"
          modify $ \st -> st { psNotes = psNotes st <> psTied st }
          when unfold $ modify $ \st -> st { psNotes = runFlow (unfoldFlow (psTime st) (psFlow st))
                                                       (psNotes st) }
        doMeasure m = mapM_ mElt $ elChildren m
        mElt elt = case ename elt of
          "barline"    -> doBarLine elt
          "attributes" -> doAttribs elt
          "direction"  -> doDirection elt
          "note"       -> doNote elt parti
          "forward"    -> doForward elt
          "backup"     -> doBackup elt
          _            -> pure ()

doBarLine :: Element -> State ParsingState ()
doBarLine elt = do
  for_ (firstChild elt "repeat") $ \rep ->
    case attr rep "direction" of
      (Just "forward")  -> pushFlow $ FwRepeat
      (Just "backward") -> pushFlow $ BwRepeat $
        maybe 2 id (attr rep "times" >>= readMaybe)
      _ -> pure ()
  for_ (firstChild elt "ending") $ \end ->
    case attr end "type" of
      (Just "start") -> for_ (attr end "number" >>= readIntList) $
        \nums -> pushFlow $ StartEnding nums
      (Just "stop") -> pushFlow $ StopEnding
      _ -> pure () -- includes "discontinue"
  where attr child name = findAttr (unqual name) child
        pushFlow flow = modify $
          \st -> st { psFlow = FM (psTime st) flow : psFlow st }

doAttribs :: Element -> State ParsingState ()
doAttribs elt = forM_ (elChildren elt) $ \att -> do
  case ename att of
    "divisions" -> for_ (readMaybe $ strContent att) $
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

doDirection :: Element -> State ParsingState ()
doDirection elt = mapM_ doSound (namedChildren elt "sound")
  where doSound sound = do
          let attr name = findAttr (unqual name) sound
              only = (attr "time-only") >>= readIntList
              pushFlow flow = modify $
                \st -> st { psFlow = FM (psTime st) flow : psFlow st }
          for_ (attr "dacapo")   $ \_    -> pushFlow $ DaCapo only
          for_ (attr "fine")     $ \_    -> pushFlow Fine
          for_ (attr "segno")    $ \sgn  -> pushFlow $ Segno sgn
          for_ (attr "dalsegno") $ \sgn  -> pushFlow $ DalSegno sgn only
          for_ (attr "coda")     $ \coda -> pushFlow $ Coda coda
          for_ (attr "tocoda")   $ \coda -> pushFlow $ ToCoda coda only
          for_ (attr "forward-repeat") $ \_ -> pushFlow FwRepeat -- does this even make sense?

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
