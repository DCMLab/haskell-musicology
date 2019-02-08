{-# LANGUAGE FlexibleContexts #-}
module Musicology.Plotting.Charts where

import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry -- hiding (x0, y0)
import Data.Colour
import qualified Graphics.Rendering.Chart.Easy as CE

import Musicology.Types
import Musicology.Internal.Helpers (whenJust)

import Control.Monad (forM_)

data PianoRollPlot n = PRPlot
  { _prNotes :: [n]
  , _prStyle :: (FillStyle, Maybe LineStyle)}
  deriving (Show, Eq)

pianoRollStyle color = (solidFillStyle color, Just $ solidLine 1.0 $ opaque black)

ptod :: (Interval p) => (Pitch p) -> Double
ptod = fromIntegral . toMidi . toInterval

plotPianoRoll :: (HasPitch n, HasTime n, PlotValue (TimeOf n)) =>
                 PianoRollPlot n -> Plot (TimeOf n) Double
plotPianoRoll (PRPlot notes style) = Plot render legend points
  where ps = (ptod . pitch) <$> notes
        ons = onset <$> notes
        offs = offset <$> notes
        render = renderPianoRoll notes style
        legend = []
        points = (ons <> offs, ((+0.5) <$> ps) <> ((\p->p-0.5) <$> ps))

renderPianoRoll :: (HasPitch n, HasTime n, PlotValue (TimeOf n)) =>
                   [n] -> (FillStyle, Maybe LineStyle)-> PointMapFn (TimeOf n) Double
                -> BackendProgram ()
renderPianoRoll notes style pmap = forM_ notes renderNote
  where pmap' = mapXY pmap
        (fStyle, lStyle) = style
        nPath note = rectPath $ Rect (pmap' (on, py+0.5)) (pmap' (off, py-0.5))
          where on  = onset note
                off = offset note
                py  = ptod $ pitch note
        renderNote note = do
          withFillStyle fStyle $ alignFillPath (nPath note) >>= fillPath
          whenJust lStyle $ \lst ->
            withLineStyle lst $ alignStrokePath (nPath note) >>= strokePath
