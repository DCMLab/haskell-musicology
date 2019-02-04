{-# LANGUAGE RankNTypes #-}
module Musicology.Grams where

import Data.Machine
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity

import Musicology.Internal.Helpers (release, processFoldable, processFoldableT)
import qualified Data.Map as M

import System.Random.MWC (Gen)
import System.Random.MWC.Distributions (bernoulli)
import Control.Monad.Primitive (PrimMonad, PrimState)

-- pointfree helpers
(.:)  = (.).(.)
(.::) = (.).(.).(.).(.)

-------------
-- n-grams --
-------------

gramsMach :: Int -> Process a [a]
gramsMach n
  | n < 1 = construct $ stop
  | otherwise = construct $ do
      inits <- replicateM n await
      go inits
        where go buf = do
                yield buf
                next <- await
                go $ tail buf ++ [next]

grams :: Foldable t => Int -> t a -> [[a]]
--grams n xs = run $ source xs ~> gramsMach n
grams n xs = processFoldable xs $ gramsMach n

---------------
-- Skipgrams --
---------------

-- basic machines
-----------------

skipgramsRMach :: (Num k, Ord k, Integral n, Monad m) =>
                  m Bool -> ([a] -> Bool) -> (a -> a -> k) -> n -> k -> ProcessT m a [a]
skipgramsRMach coin pred cost n k = unfoldPlan [] go
  where -- go :: [(n, k, [a])] -> Plan...
        go pfxs = do
          candidate     <- await
          let oldClosed  = filter (\p -> totalCost p candidate <= k) pfxs
          extendable    <- filterM (doesExtend candidate) oldClosed
          let extended   = mkPrefix candidate : fmap (extendPrefix candidate) extendable
              pfxs'      = oldClosed ++ filter (not . pfxComplete) extended -- two filters in one call (look up)
          mapM_ yieldGram $ filter pfxComplete extended
          return pfxs'
        mkPrefix x = (n-1, 0, [x])
        totalCost (_, k, prev:_) x = k + cost prev x
        extendPrefix x (n, k, p:fx) = (n-1, k + cost p x, x:p:fx)
        pfxComplete (n, _, _) = n==0
        doesExtend cand (_, _, pfx) = do
          isHeads <- lift coin
          return $ (pred $ cand:pfx) && isHeads
        yieldGram (_, _, gram) = yield $ reverse gram

skipgramsRSMach :: (Num k, Ord k, Integral n, Monad m) =>
                   m Bool -> ([a] -> Bool) -> (a -> a -> k) -> n -> k -> ProcessT m a [a]
skipgramsRSMach coin pred cost n k = unfoldPlan ([], 0::Int, M.empty) go
  where -- go :: [(n, k, [a], Int)] -> Plan...
        go (pfxs, i, queue) = do
          candidate     <- await <|> (mapM_ (mapM_ yield) (M.elems queue) *> stop)
          let oldClosed  = filter (\p -> totalCost p candidate <= k) pfxs
              gate       = minimum $ fmap prefixOnset oldClosed
              (q', rels) = release queue gate
          mapM_ (mapM_ yield) rels
          extendable    <- filterM (doesExtend candidate) oldClosed
          let extended   = mkPrefix candidate : fmap (extendPrefix candidate) extendable
              pfxs'      = oldClosed ++ filter (not . pfxComplete) extended
              queue'     = enqueueGrams q' $ filter pfxComplete extended
          return (pfxs', i+1, queue')
          where
            mkPrefix x = (n-1, 0, [x], i)
        totalCost (_, k, prev:_, _) x = k + cost prev x
        prefixOnset (_, _, _, i) = i
        extendPrefix x (n, k, p:fx, i) = (n-1, k + cost p x, x:p:fx, i)
        pfxComplete (n, _, _, _) = n==0
        doesExtend cand (_, _, pfx, _) = do
          isHeads <- lift coin
          return $ (pred $ cand:pfx) && isHeads
        enqueueGrams q pfxs = M.unionWith (++) q gramq
          where grams = map (\(_, _, pfx, i) -> (i, [reverse pfx])) pfxs
                gramq = M.fromListWith (++) grams

-- special cases: deterministic
-------------------------------

skipgramsMach :: (Num k, Ord k, Integral n) =>
                 ([a] -> Bool) -> (a -> a -> k) -> n -> k -> Process a [a]
skipgramsMach = skipgramsRMach (return True)

skipgramsSMach :: (Num k, Ord k, Integral n) =>
                  ([a] -> Bool) -> (a -> a -> k) -> n -> k -> Process a [a]
skipgramsSMach = skipgramsRSMach (return True)

skipgrams :: (Foldable t, Num k, Ord k, Integral n) =>
             t a -> ([a] -> Bool) -> (a -> a -> k) -> n -> k -> [[a]]
skipgrams xs = processFoldable xs .:: skipgramsMach

skipgramsS :: (Foldable t, Num k, Ord k, Integral n) =>
              t a -> ([a] -> Bool) -> (a -> a -> k) -> n -> k -> [[a]]
skipgramsS xs  = processFoldable xs .:: skipgramsSMach

-- special cases: stochastic sampling
-------------------------------------

mkCoin :: (PrimMonad m, Integral n) => Double -> n -> Gen (PrimState m) -> m Bool
mkCoin p n gen = bernoulli bias gen
  where bias = p ** (1 / fromIntegral (n-1))

skipgramsR :: (Foldable t, Num k, Ord k, Integral n, PrimMonad m) =>
              t a -> Double -> Gen (PrimState m) ->
              ([a] -> Bool) -> (a -> a -> k) -> n -> k -> m [[a]]
skipgramsR xs 1.0 _ pred cost n = return . skipgrams xs pred cost n
skipgramsR xs p gen pred cost n =
  processFoldableT xs . skipgramsRMach (mkCoin p n gen) pred cost n

skipgramsRS :: (Foldable t, Num k, Ord k, Integral n, PrimMonad m) =>
               t a -> Double -> Gen (PrimState m) ->
               ([a] -> Bool) -> (a -> a -> k) -> n -> k -> m [[a]]
skipgramsRS xs 1.0 _ pred cost n = return . skipgramsS xs pred cost n
skipgramsRS xs p gen pred cost n =
  processFoldableT xs . skipgramsRSMach (mkCoin p n gen) pred cost n

-- special cases: index-based cost
----------------------------------

enum xs = zip xs [0..]
indexCost (x,i) (y,j) = j-i-1
mune = map (map fst)

indexSkipgrams :: [a] -> Int -> Int -> [[a]]
indexSkipgrams xs = mune .: skipgrams (enum xs) (const True) indexCost

indexSkipgramsR :: PrimMonad m =>
                   [a] -> Double -> Gen (PrimState m) -> Int -> Int -> m [[a]]
indexSkipgramsR xs p gen =
  (mune <$>) .: skipgramsR (enum xs) p gen (const True) indexCost

indexSkipgramsS :: [a] -> Int -> Int -> [[a]]
indexSkipgramsS xs = mune .: skipgramsS (enum xs) (const True) indexCost

indexSkipgramsRS :: PrimMonad m =>
                    [a] -> Double -> Gen (PrimState m) -> Int -> Int -> m [[a]]
indexSkipgramsRS xs p gen =
  (mune <$>) .: skipgramsRS (enum xs) p gen (const True) indexCost

---------------
-- Polygrams --
---------------
