{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Musicology.Internal.Helpers where

import Musicology.Core

import Frames
import qualified Data.Vinyl.Functor
import Data.Vinyl.Class.Method (RecMapMethod)
import Data.Vinyl.Core (RecordToList)
import Data.Proxy

-- import Data.VectorSpace

-- import qualified Data.Sequence as S
import qualified Data.List as L
import qualified Data.Map as M

import Data.Machine
import Control.Monad.Identity
import Control.Monad.State
import Control.Applicative ((<|>))

-- general tools

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust _        _ = return ()

-- machine helpers
processFoldable :: Foldable t => t a -> MachineT Identity (Is a) b -> [b]
processFoldable xs p = run $ source xs ~> p

processFoldableT :: (Monad m, Foldable t) => t a -> ProcessT m a b -> m [b]
processFoldableT xs p = runT $ source xs ~> p

-- frames helpers

showRow :: (RecMapMethod Show ElField a, RecordToList a)
         => Record a -> String
showRow row = L.intercalate "\t" $ showFields row

showHeader :: forall a . (ColumnHeaders a) => Frame (Record a) -> String
showHeader frame = L.intercalate "\t" $ columnHeaders (Proxy :: Proxy (Record a))

previewFrame :: (RecMapMethod Show ElField a, RecordToList a, ColumnHeaders a)
             => Int -> Frame (Record a) -> IO ()
previewFrame n fr = do
  putStrLn $ showHeader fr
  preview [0..n-1]
  putStrLn "..."
  preview [l-n..l-1]
  where l = frameLength fr
        preview is = mapM_ (putStrLn . showRow . frameRow fr) is

viewFrame :: (RecMapMethod Show ElField a, RecordToList a, ColumnHeaders a)
          => Frame (Record a) -> IO ()
viewFrame frame = do
  putStrLn $ showHeader frame
  mapM_ (putStrLn . showRow) frame

-- release :: Ord k => M.Map k v -> k -> (M.Map k v, [v])
-- release q gate = (q', M.elems rels)
--   where (rels, q') = M.spanAntitone (<=gate) q

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _cmp [] ys = ys
mergeBy _cmp xs [] = xs
mergeBy cmp allx@(x:xs) ally@(y:ys)
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys

release :: Ord k => M.Map k v -> k -> (M.Map k v, [v])
release q gate = (q', M.elems rels)
  where (rels, q') = M.spanAntitone (<=gate) q

intFills :: (Eq p, Interval p) => p -> p -> Bool
intFills pm ph = odir == EQ && pm /= zeroV && pm /= ph ||
                 odir /= EQ && dir1 == odir && dir2 == odir
  where odir = direction ph
        dir1 = direction pm
        dir2 = direction (ph^-^pm)

rerunStateT :: (m (a, s) -> n (b, t)) -> (t -> s) -> StateT s m a -> StateT t n b
rerunStateT f r m = StateT $ f . runStateT m . r
