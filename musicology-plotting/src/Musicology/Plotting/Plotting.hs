{-# LANGUAGE OverloadedStrings #-}
module Musicology.Plotting.Plotting where

import Musicology.Types

import Graphics.Vega.VegaLite
import Data.Aeson as J
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Monoid ((<>))

import System.IO.Temp (emptySystemTempFile)
import Web.Browser
import Html
import qualified Html.Attribute as A
import qualified Data.Text.Lazy.IO as TL

ppSpec = BS8.putStrLn . encodePretty . fromVL

writeSpec fn = BS8.writeFile fn . encode . fromVL

viewPlot :: VegaLite -> IO ()
viewPlot plot = do
  fnJ <- emptySystemTempFile "musicology-plot.json"
  fnH <- emptySystemTempFile "musicology-plot.html"
  writeSpec fnJ plot
  TL.writeFile fnH $ renderText $ vegaEmbed fnJ
  openBrowser fnH
  return ()

fixRatios :: Value -> Value
fixRatios (J.Object o) = J.Object $ fixRatio "onset" $ fixRatio "offset" o
  where fixRatio name o = HM.adjust fix name o
        fix (J.Object rat) =
          let (J.Number num) = HM.lookupDefault (J.Number 0) "numerator" rat
              (J.Number den) = HM.lookupDefault (J.Number 1) "denominator" rat
              in toJSON $ num / den
        fix v = v
fixRatios v = v

pianorollView :: (ToJSON p, ToJSON t, Pitch p) =>
             [(Note p t)] -> Maybe T.Text -> [(VLProperty, VLSpec)] 
pianorollView notes colorName =
  let pitches = fmap (fromIntegral . toMidi . pitch) notes
      minp = minimum pitches
      maxp = maximum pitches
      (Array nj) = (toJSON notes)
      vals = dataFromJson $ Array $ fixRatios <$> nj 
      enc  = encoding
             . position X [PName "onset", PmType Quantitative]
             . position X2 [PName "offset", PmType Quantitative]
             . position Y [PName "pitch", PmType Ordinal,
                           PScale [SDomain $ DNumbers (reverse [minp..maxp])]]
             . tooltip [TName "pitch", TmType Quantitative]
             . maybe id (\cn -> color [MString cn]) colorName
             -- . position Y [PName "pu", PmType Quantitative]
             -- . position Y2 [PName "pl", PmType Quantitative]
             -- . position Y [PName "pitch", PmType Quantitative, PBin [Step 1]]
      -- tran = transform
      --        . calculateAs "datum.pitch+0.5" "pu"
      --        . calculateAs "datum.pitch-0.5" "pl"
  in [mark Rect [], vals [], enc []] -- tran []

pianoroll :: (ToJSON p, ToJSON t, Pitch p) =>
             [(Note p t)] -> VegaLite
pianoroll notes = toVegaLite $ [width 800, height 300, sel []] ++ pianorollView notes Nothing
  where sel = selection . select "grid" Interval [ BindScales ]

plotpolysView :: (ToJSON p, ToJSON t, Pitch p) =>
             [Note p t] -> [[Note p t]] -> [(VLProperty, VLSpec)]
plotpolysView notes polys = [layer [background, foreground]]
  where background = fromVL $ toVegaLite $ pianorollView notes (Just "#888888")
        polyJson name ns = fmap (extend . fixRatios) jNotes
          where (Array jNotes) = toJSON ns
                extend (J.Object note) = J.Object $ HM.insert "name" (String name) note
        notedat = polyJson "notes" notes
        polydat = V.concat $ zipWith (polyJson . mkn "poly ") [1..] polys
        mkn pfx = (pfx <>) . T.pack . show
        vals    = dataFromJson (Array $ polydat)
        pitches = fmap (fromIntegral . toMidi . pitch) notes
        minp    = minimum pitches
        maxp    = maximum pitches
        enc     = encoding
                  . position X  [PName "onset",  PmType Quantitative]
                  . position X2 [PName "offset", PmType Quantitative]
                  . position Y  [PName "pitch",  PmType Ordinal,
                                 PScale [SDomain $ DNumbers (reverse [minp..maxp])]]
                  . color [MName "name", MmType Nominal]
                  . tooltip [TName "pitch", TmType Quantitative]
        foreground = fromVL $ toVegaLite [mark Rect [], vals [], enc [], sel []]
        sel = selection . select "grid" Interval [ BindScales ]

plotpolys' :: (ToJSON p, ToJSON t, Pitch p) =>
             [Note p t] -> [[Note p t]] -> VegaLite
plotpolys' notes polys = toVegaLite $ [width 800, height 300, sel []] ++ plotpolysView notes polys
  where sel = selection . select "grid" Interval [ BindScales ]

plotpolys :: (ToJSON p, ToJSON t, Pitch p) =>
             [Note p t] -> [[[Note p t]]] -> VegaLite
plotpolys notes polys = plotpolys' notes $ concat <$> polys

nüx :: T.Text
nüx = ""

vegaEmbed fn = html_
  ( head_
    ( title_ ("Plot"::T.Text) #
      script_A (A.src_ ("https://cdn.jsdelivr.net/npm/vega@3"::T.Text)) nüx #
      script_A (A.src_ ("https://cdn.jsdelivr.net/npm/vega-lite@2"::T.Text)) nüx #
      script_A (A.src_ ("https://cdn.jsdelivr.net/npm/vega-embed@3"::T.Text)) nüx) #
    body_
    ( div_A (A.id_ ("vis"::T.Text)) nüx #
      script_A (A.type_ ("text/javascript"::T.Text)) (Raw (vegaJsText fn))))

vegaJsText :: FilePath -> T.Text
vegaJsText fn = "var spec = \"" <> T.pack fn <> "\";vegaEmbed('#vis', spec).then(function(result) {}).catch(console.error);"
