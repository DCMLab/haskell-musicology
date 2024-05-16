{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Musicology.MusicXML
  ( parseWithIds, parseWithoutIds, idfy
  , XmlNote(..), XmlRecord, AsWritten(..)
  , xmlNoteToRecord, notesToFrame
  , asNoteWritten, asNoteWithIdWritten
  , asNoteHeard, asNoteWithIdHeard
  , xmlNotes, parseScore
  ) where

--import Text.XML.Light
--import Text.XML.Light.Lexer (XmlSource)
import Text.XML
import Text.XML.Cursor
import Frames
import Frames.InCore (VectorFor)
import Data.Vinyl.Core (Rec(..))
import qualified Data.Vector as V
import qualified Data.Map as M

-- import Data.ByteString
import Data.Foldable (for_)
import Data.List (sort, find, sortOn, uncons)
import Data.Maybe (isJust, catMaybes, listToMaybe, fromMaybe)
import Data.Ratio
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Control.Applicative ((<|>))
import Control.Monad.State
import Control.Monad (mapM_, when, unless, forM_)
import Text.Read (readMaybe)
import Lens.Micro

import Musicology.Core

import qualified Debug.Trace as D
import Musicology.Time

-- helpers
----------

hasAttrib :: Element -> Name -> Bool
hasAttrib (Element _ attrs _) name = M.member name attrs -- any ((==k) . attrKey) (elementAttributes el)

setAttrib :: Element -> Name -> Text -> Element
setAttrib (Element name attrs children) k v = Element name attrs' children
  where attrs' = M.insert k v attrs

ename :: Element -> Name
ename = elementName

getContent :: Node -> Maybe T.Text
getContent (NodeContent t) = Just t
getContent _ = Nothing

getElt :: Node -> Maybe Element
getElt (NodeElement elt) = Just elt
getElt _ = Nothing

strContent :: Element -> String
strContent elt = T.unpack $ mconcat $ catMaybes $ getContent <$> elementNodes elt

firstInt :: Element -> Name -> Maybe Int
firstInt elt subname = do
  sub <- listToMaybe $ namedChildren elt subname
  readMaybe $ strContent sub

firstInt' :: Element -> Name -> Int -> Int
firstInt' elt subname def = fromMaybe def $ firstInt elt subname

hasChild :: Element -> Name -> Bool
hasChild elt name = not $ null $ fromNode (NodeElement elt) $/ element name

firstChild :: Element -> Name -> Maybe Element
firstChild elt name = listToMaybe $ namedChildren elt name

namedChildren :: Element -> Name -> [Element]
namedChildren elt name = catMaybes $ getElt . node <$> (fromNode (NodeElement elt) $/ element name)

allChildren :: Element -> [Element]
allChildren (Element _ _ nodes) = catMaybes $ getElt <$> nodes

attrIs :: Element -> Name -> T.Text -> Bool
attrIs elt name val = M.lookup name (elementAttributes elt) == Just val

readIntList :: T.Text -> Maybe [Int]
readIntList str = readMaybe $ "[" <> T.unpack str <> "]"

-- generating ids
-----------------

parseWithIds :: Bool -> LT.Text -> Maybe Document
parseWithIds keep input = do
  doc <- parseWithoutIds input
  pure $ doc {documentRoot = evalState (addIds $ documentRoot doc) 0}
  where next :: State Int Int
        next = get >>= \n -> put (n+1) >> pure n
        qID = "xml:id" --QName "id" Nothing (Just "xml")
        addIds elt = do
          e <- elt'
          c <- cont'
          pure $ e { elementNodes = c }
          where elt' = if nameLocalName (elementName elt) == "note" && (not keep || not (hasAttrib elt qID))
                       then next >>= \i -> pure $ setAttrib elt qID ("note" <> T.pack (show i))
                       else pure elt
                cont' = mapM contentIds (elementNodes elt)
        contentIds (NodeElement e) = NodeElement <$> addIds e
        contentIds other = pure other

parseWithoutIds :: LT.Text -> Maybe Document
parseWithoutIds txt = either (const Nothing) Just $ parseText def txt

idfy :: Bool -> LT.Text -> LT.Text
idfy keep input = maybe "" (renderText def) $ parseWithIds keep input

-- note list
------------

data XmlNoteW = XmlNoteW
  { _onsetW :: Ratio Int
  , _offsetW :: Ratio Int
  , _pitchW :: SPitch
  , _partW :: Int
  , _idW :: Maybe String
  }
  deriving (Eq, Ord, Show)

data XmlNote = XmlNote
  { _onsetWritten :: Ratio Int
  , _offsetWritten :: Ratio Int
  , _onsetHeard :: Ratio Int
  , _offsetHeard :: Ratio Int
  , _pitch :: SPitch
  , _part :: Int
  , _id :: Maybe String
  }
  deriving (Eq, Ord, Show)

newtype AsWritten = AsWritten XmlNote
  deriving (Eq, Ord, Show)

instance Timed XmlNote where
  type TimeOf XmlNote = Ratio Int

instance HasTime XmlNote where
  onsetL f note = fmap (\on' -> note { _onsetHeard = on' }) (f $ _onsetHeard note)
  offsetL f note = fmap (\off' -> note { _offsetHeard = off' }) (f $ _offsetHeard note)

instance Timed AsWritten where
  type TimeOf AsWritten = Ratio Int

instance HasTime AsWritten where
  onsetL f (AsWritten note) =
    fmap (\on' -> AsWritten $ note { _onsetWritten = on' }) (f $ _onsetWritten note)
  offsetL f (AsWritten note) =
    fmap (\off' -> AsWritten $ note { _offsetWritten = off' }) (f $ _offsetWritten note)

instance Pitched XmlNote where
  type IntervalOf XmlNote = SInterval
  type ReTypeInterval XmlNote p2 = XmlNote

instance HasPitch XmlNote where
  pitchL :: Lens' XmlNote SPitch
  pitchL f note = fmap updatePitch (f $ _pitch note)
    where updatePitch p = note { _pitch  = p }

instance Pitched AsWritten where
  type IntervalOf AsWritten = SInterval
  type ReTypeInterval AsWritten p2 = AsWritten

instance HasPitch AsWritten where
  pitchL :: Lens' AsWritten SPitch
  pitchL f (AsWritten note) = fmap updatePitch (f $ _pitch note)
    where updatePitch p = AsWritten $ note { _pitch  = p }

type instance VectorFor (Ratio Int) = V.Vector
type instance VectorFor (Maybe String) = V.Vector
type instance VectorFor SPitch = V.Vector

type XmlRecord = Record '[ "onsetWritten" :-> Ratio Int
                         , "offsetWritten" :-> Ratio Int
                         , "onsetHeard" :-> Ratio Int
                         , "offsetHeard" :-> Ratio Int
                         , "pitch" :-> SPitch
                         , "part" :-> Int
                         , "id" :-> Maybe String
                         ]

xmlNoteToRecord :: XmlNote -> XmlRecord
xmlNoteToRecord (XmlNote onW offW onH offH p part id) =
  onW &: offW &: onH &: offH &: p &: part &: id &: RNil

notesToFrame :: [XmlNote] -> Frame XmlRecord
notesToFrame notes = toFrame $ map xmlNoteToRecord notes

asNoteWritten :: XmlNote -> Note SInterval (Ratio Int)
asNoteWritten (XmlNote on off _ _ p _ _) = Note p on off

asNoteWithIdWritten :: XmlNote -> NoteId SInterval (Ratio Int) (Maybe String)
asNoteWithIdWritten (XmlNote on off _ _ p _ id) = NoteId p on off id

asNoteHeard :: XmlNote -> Note SInterval (Ratio Int)
asNoteHeard (XmlNote _ _ on off p _ _) = Note p on off

asNoteWithIdHeard :: XmlNote -> NoteId SInterval (Ratio Int) (Maybe String)
asNoteWithIdHeard (XmlNote _ _ on off p _ id) = NoteId p on off id

-- musical control flow
-----------------------

data FlowMarker t = FM t FlowCommand -- (Maybe [Int])
  deriving (Eq, Ord, Show)

-- order: closing low, closing high, opening high, opening low
data FlowCommand = BwRepeat Int
                 | StopEnding
                 | Fine (Maybe [Int])
                 | DaCapo (Maybe [Int])
                 | DalSegno Text (Maybe [Int])
                 | ToCoda Text (Maybe [Int])
                 | Coda Text
                 | Segno Text
                 | StartEnding [Int]
                 | FwRepeat
  deriving (Eq, Ord, Show)

type RepStack t = [(t, Int, [FlowMarker t])]

data FlowState t = FS
  { fsNow :: t -- current time
  , fsMarkers :: [FlowMarker t] -- all markers from here to end of piece
  , fsSegno :: M.Map Text ([FlowMarker t], t, RepStack t) -- segnos with their repetition stacks
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
unfoldFlow end markers = foldJumps $ fsJumps $ execState doFlow init
  where foldJumps lst = fj end lst []
        fj t [] acc = (0%1, t) : acc
        fj t (End e:rst) acc = fj e rst acc
        fj t (Jump t1 t2:rst) acc = fj t1 rst ((t2,t) : acc)
        markersSorted = sort markers -- sort respects marker precedence (see above)
        beginning = (0%1, 1, markersSorted) -- rep stack item for beginning of the piece
        init = FS (0%1) markersSorted M.empty [beginning] M.empty []
        restore markers t stack =
          modify (\st -> st { fsMarkers = markers, fsNow = t, fsStack = stack
                            , fsJumps = Jump (fsNow st) t : fsJumps st })
        reenter = restore markersSorted (0%1) [beginning]
        pushRepeat = modify $ \st -> st { fsStack = (fsNow st, 1, fsMarkers st) : fsStack st }
        popRepeat = modify $ \st -> st { fsStack = maybe [] snd $ uncons $ fsStack st }
        goRepeat = do
          stack <- gets fsStack
          case uncons stack of -- repeat from repeat sign or beginning?
            Just ((t, n, markers), rst) -> restore markers t ((t, n+1, markers) : rst)
            Nothing -> D.traceM "Warning: Don't know where to repeat from. Missing forward repeat?"
              --  restore markersSorted (0%1) [(0%1, 2, markersSorted)]
        skipUntil com = do
          markers <- gets fsMarkers
          let remaining = dropWhile (\(FM _ c) -> c /= com) markers
              (FM t _) = fromMaybe (FM end com) $ listToMaybe remaining
          modify $ \st -> st { fsMarkers = remaining, fsNow = t
                             , fsJumps = Jump (fsNow st) t : fsJumps st }
        currentRep = gets $ maybe 1 (\(_, n, _) -> n) . listToMaybe . fsStack
        pass counts marker = M.findWithDefault 1 marker counts
        checkTimes Nothing _ action = action -- no times? always run
        checkTimes (Just times) marker action = do
          gc <- gets fsGCount
          when (pass gc marker `elem` times) action
        checkTimes' Nothing marker action = do -- no times? only run on second pass or higher
          gc <- gets fsGCount
          when (pass gc marker > 1) action
        checkTimes' (Just times) m a = checkTimes (Just times) m a
        doFlow = do
          -- get >>= D.traceM . show
          mm <- nextMarker
          for_ mm $ \marker@(FM t com) -> do
            case com of
              FwRepeat       -> pushRepeat
              BwRepeat n     -> do
                rep <- currentRep
                if rep < n then goRepeat else popRepeat
              StartEnding ns -> do
                n <- currentRep
                if n `elem` ns then pure () else skipUntil StopEnding
              StopEnding     -> pure ()
              Fine times     -> checkTimes' times marker $
                                modify $ \st -> st { fsMarkers = [] -- stops recursion
                                                   , fsJumps = End (fsNow st) : fsJumps st }
              DaCapo times   -> checkTimes times marker reenter
              Segno name     -> modify $ \st ->
                let now = fsNow st
                    stack = fsStack st
                    markers = fsMarkers st
                    segnos = M.insert name (markers, now, stack) $ fsSegno st
                in st { fsSegno = segnos }
              DalSegno name t -> checkTimes t marker $ get >>= \st ->
                for_ (M.lookup name $ fsSegno st) $ \(m, t, s) -> restore m t s
              Coda name      -> pure ()
              ToCoda name t  -> checkTimes' t marker $ skipUntil $ Coda name
            doFlow

runFlow :: (HasTime n, TimeOf n ~ t, Num t, Ord t) => [(t,t)] -> [n] -> [n]
runFlow blocks notes = snd $ foldl nextBlock (0, []) blocks
  where nextBlock (now,acc) (t1,t2) = (now', acc')
          where shift = (+(now-t1))
                now'  = shift t2
                nts   = filter (\n -> (offset n > t1) && (onset n < t2)) notes
                acc'  = acc <> fmap ((onsetL %~ shift) . (offsetL %~ shift)) nts

runFlowXml :: [(Ratio Int,Ratio Int)] -> [XmlNoteW] -> [XmlNote]
runFlowXml blocks notes = snd $ foldl nextBlock (0, []) blocks
  where nextBlock (now,acc) (t1,t2) = (now', acc')
          where shift = (+(now-t1))
                now'  = shift t2
                nts   = filter (\n -> (_offsetW n > t1) && (_onsetW n < t2)) notes
                acc'  = acc <> fmap (\(XmlNoteW on off p i pt) -> XmlNote on off (shift on) (shift off) p i pt) nts


-- parsing to notelist
----------------------

data ParsingState n = PS
  { psNotes :: [n]
  , psDiv :: Int
  , psTime :: Ratio Int
  , psPrevTime :: Ratio Int
  , psTrans :: SInterval
  , psTied :: [XmlNoteW]
  , psTimeSig :: TimeSignature
  , psSigs :: [(Ratio Int, TimeSignature)]
  , psFirstBar :: Bool
  , psFlow :: [FlowMarker (Ratio Int)]
  } deriving Show

xmlNotes :: Maybe Document -> [XmlNote]
xmlNotes = maybe [] (fst . parseScore)

parseScore :: Document -> ([XmlNote], [[(Ratio Int, TimeSignature)]])
parseScore (Document _ root _i)
  | ename root == "score-partwise" = (sortOn _onsetHeard $ reverse notes, sigmaps)
  | otherwise = ([], [])
  where parts = uncurry parsePart <$> zip (namedChildren root "part") [1..]
        notes = concatMap fst parts
        sigmaps = snd <$> parts

parsePart :: Element -> Int -> ([XmlNote], [(Ratio Int, TimeSignature)])
parsePart part parti = (notes, sigs)
  where init = PS [] 1 (0%1) (0%1) unison [] (TS 4 4) [] True []
        final = execState doPart init
        notes = runFlowXml (unfoldFlow (psTime final) (psFlow final)) (psNotes final)
        sigs = reverse $ psSigs final
        doPart = do
          mapM_ doMeasure $ namedChildren part "measure"
          modify $ \st -> st { psNotes = psNotes st <> psTied st }
        doMeasure m = do
          mapM_ mElt $ allChildren m
          st <- get
          when (psFirstBar st) $ do
            let firstSigOn = if psTime st < measureDuration (psTimeSig st)
                  then psTime st
                  else 0%1
            put $ st { psFirstBar = False, psSigs = [(firstSigOn, psTimeSig st)]}
        mElt elt = case ename elt of
          "barline"    -> doBarLine elt
          "attributes" -> doAttribs elt
          "direction"  -> doDirection elt
          "note"       -> doNote elt parti
          "forward"    -> doForward elt
          "backup"     -> doBackup elt
          _            -> pure ()

doBarLine :: Element -> State (ParsingState XmlNoteW) ()
doBarLine elt = do
  for_ (firstChild elt "repeat") $ \rep ->
    case attr rep "direction" of
      Just "forward"  -> pushFlow FwRepeat
      Just "backward" -> pushFlow $ BwRepeat $
        fromMaybe 2 (attr rep "times" >>= readMaybe . T.unpack)
      _ -> pure ()
  for_ (firstChild elt "ending") $ \end ->
    case attr end "type" of
      Just "start" -> for_ (attr end "number" >>= readIntList) $
        \nums -> pushFlow $ StartEnding nums
      Just "stop" -> pushFlow StopEnding
      _ -> pure () -- includes "discontinue"
  where attr child name = M.lookup name (elementAttributes child)
        pushFlow flow = modify $
          \st -> st { psFlow = FM (psTime st) flow : psFlow st }

doAttribs :: Element -> State (ParsingState XmlNoteW) ()
doAttribs elt = forM_ (allChildren elt) $ \att ->
  case ename att of
    "divisions" -> for_ (readMaybe $ strContent att) $
                   \div -> modify $ \st -> st { psDiv = div }
    "transpose" -> let maybeChrom = firstInt att "chromatic"
                       dia   = firstInt' att "diatonic" 0
                       octs  = firstInt' att "octave-change" 0
                   in case maybeChrom of
                        Nothing -> pure ()
                        Just chrom  -> modify $
                          \st -> st { psTrans = spelledDiaChrom dia chrom ^+^ octs *^ octave}
    "time" -> let num = firstInt' att "beats" 4
                  denom = firstInt' att "beat-type" 4
                  ts = TS num denom
              in modify $ \st -> st { psSigs = (psTime st, ts) : psSigs st, psTimeSig = ts}
    _           -> pure ()

doDirection :: Element -> State (ParsingState XmlNoteW) ()
doDirection elt = mapM_ doSound (namedChildren elt "sound")
  where doSound sound = do
          let attr name = M.lookup name (elementAttributes sound)
              only = attr "time-only" >>= readIntList
              pushFlow flow = modify $
                \st -> st { psFlow = FM (psTime st) flow : psFlow st }
          for_ (attr "dacapo")   $ \_    -> pushFlow $ DaCapo only
          for_ (attr "fine")     $ \_    -> pushFlow $ Fine only
          for_ (attr "segno")    $ \sgn  -> pushFlow $ Segno sgn
          for_ (attr "dalsegno") $ \sgn  -> pushFlow $ DalSegno sgn only
          for_ (attr "coda")     $ \coda -> pushFlow $ Coda coda
          for_ (attr "tocoda")   $ \coda -> pushFlow $ ToCoda coda only
          for_ (attr "forward-repeat") $ \_ -> pushFlow FwRepeat -- does this even make sense?

noteNames :: String -> Maybe (Accidental -> Int -> SPitch)
noteNames "C" = Just c
noteNames "D" = Just d
noteNames "E" = Just e
noteNames "F" = Just f
noteNames "G" = Just g
noteNames "A" = Just a
noteNames "B" = Just b
noteNames _   = Nothing

doNote :: Element -> Int -> State (ParsingState XmlNoteW) ()
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
    pitchName <- noteNames $ strContent step
    let pitchWritten = pitchName (Acc alt) oct
        pitch = pitchWritten +^ psTrans st
        -- dia   = psTransDia   st + dia'   + oct * 7
        -- chrom = psTransChrom st + chrom' + oct * 12 + alt

    -- id
    let id = fmap T.unpack $ M.lookup "id" (elementAttributes note) <|>
                             M.lookup "xml:id" (elementAttributes note)

    -- ties
    let ties = namedChildren note "tie"
        anyTieOfType val = any (\tie -> attrIs tie "type" val) ties

    -- tie stop?
    let (tied, onset', id') = if anyTieOfType "stop"
          then let continued (XmlNoteW _ toff tpitch _ _) =
                     toff == onset && tpitch == pitch
                   start = find continued (psTied st) in
                 case start of
                   Nothing -> (psTied st, onset, id)
                   Just n@(XmlNoteW ton _ _ _ tid) ->
                     ( filter (/=n) (psTied st)
                     , ton
                     , tid <|> id )
          else (psTied st, onset, id)

    -- tie start?
    let newNote = XmlNoteW onset' offset pitch parti id'
    if anyTieOfType "start"
      then pure $ modify $ \st -> st { psTied = newNote : tied }
      else pure $ modify $ \st -> st { psNotes = newNote : psNotes st, psTied = tied }
    -- let newNote = XmlNote onset offset dia chrom parti id
    -- pure $ modify $ \st -> st { psNotes = newNote : psNotes st }

doForward :: Element -> State (ParsingState XmlNoteW) ()
doForward elt = do
  st <- get
  let div = psDiv st
      t   = psTime st
      t'  = t + firstInt' elt "duration" 0 % (div * 4)
  put st { psTime = t', psPrevTime = t' }

doBackup :: Element -> State (ParsingState XmlNoteW) ()
doBackup elt = do
  st <- get
  let div = psDiv st
      t   = psTime st
      t'  = t - firstInt' elt "duration" 0 % (div * 4)
  put st { psTime = t', psPrevTime = t' }
