{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Musicology.GramsStreaming where

import           Musicology.Internal.Helpers    ( release )
import qualified Data.Map                      as M
import qualified Data.List                     as L
import           Safe.Foldable                  ( minimumMay )

import           Streaming as S
import qualified Streaming.Prelude              as S

import           Control.Concurrent
import           Control.Monad                  ( forever
                                                , filterM
                                                )
-- import           Control.Monad.Trans            ( lift )

skipgramsR
  :: forall k n m r a
   . (Num k, Ord k, Integral n, Monad m)
  => m Bool
  -> ([a] -> Bool)
  -> (a -> a -> k)
  -> n
  -> k
  -> Stream (Of a) m r
  -> Stream (Of [a]) m r
skipgramsR coin pred cost n k input = S.for scanned S.each
 where
  scanned = S.drop 1 $ S.scanM newGrams (return ([], [])) (return . fst) input
  newGrams :: ([[a]], [(n, k, [a])]) -> a -> m ([[a]], [(n, k, [a])])
  newGrams (_, pfxs) candidate = do
    let oldClosed = filter (\p -> totalCost p candidate <= k) pfxs
    extendable <- filterM (doesExtend candidate) oldClosed
    let extended =
          mkPrefix candidate : fmap (extendPrefix candidate) extendable
        (complete, open) = L.partition pfxComplete extended
        pfxs'            = oldClosed ++ open
    return (yieldGram <$> complete, pfxs')
  mkPrefix x = (n - 1, 0, [x])
  totalCost (_, k, prev : _) x = k + cost prev x
  extendPrefix x (n, k, p : fx) = (n - 1, k + cost p x, x : p : fx)
  pfxComplete (n, _, _) = n == 0
  doesExtend cand (_, _, pfx) = do
    isHeads <- coin
    return $ isHeads && (pred $ cand : pfx)
  yieldGram (_, _, gram) = reverse gram

skipgramsRS
  :: forall n k m r a
   . (Num k, Ord k, Integral n, Monad m)
  => m Bool
  -> ([a] -> Bool)
  -> (a -> a -> k)
  -> n
  -> k
  -> Stream (Of a) m r
  -> Stream (Of [a]) m r
skipgramsRS coin pred cost n k input =
  S.for unfolded (\xs -> S.for xs S.each)
 where
  unfolded = S.unfoldr unfoldItem $ Right ([], 0, M.empty, input)
  unfoldItem
    :: Either r ([(n, k, [a], Int)], Int, M.Map Int [[a]], Stream (Of a) m r)
    -> m
         ( Either r
             ( Stream (Of [[a]]) m ()
             , Either r ([(n, k, [a], Int)], Int, M.Map Int [[a]], Stream (Of a) m r)
             )
         )
  unfoldItem (Left r)                       = return $ Left r
  unfoldItem (Right (pfxs, i, queue, items)) = do
    uc <- (S.next items)
    case uc of
      Left r -> return $ Right (S.each $ M.elems queue, Left r)
      Right (candidate, rest) -> do
        let oldClosed  = filter (\p -> totalCost p candidate <= k) pfxs
            gate       = minimumMay $ fmap prefixOnset oldClosed
            (q', rels) = case gate of
              Just g  -> release queue g
              Nothing -> (M.empty, M.elems queue)
        extendable <- filterM (doesExtend candidate) oldClosed
        let extended =
              mkPrefix candidate : fmap (extendPrefix candidate) extendable
            (complete, open) = L.partition pfxComplete extended
            pfxs'            = oldClosed ++ open
            queue'           = enqueueGrams q' complete
        return $ Right (S.each rels, Right (pfxs', i + 1, queue', rest))
    where mkPrefix x = (n - 1, 0, [x], i)
  totalCost (_, k, prev : _, _) x = k + cost prev x
  prefixOnset (_, _, _, i) = i
  extendPrefix x (n, k, p : fx, i) = (n - 1, k + cost p x, x : p : fx, i)
  pfxComplete (n, _, _, _) = n == 0
  doesExtend cand (_, _, pfx, _) = do
    isHeads <- coin
    return $ (pred $ cand : pfx) && isHeads
  enqueueGrams q pfxs = M.unionWith (++) q gramq
   where
    grams = map (\(_, _, pfx, i) -> (i, [reverse pfx])) pfxs
    gramq = M.fromListWith (++) grams
  getResult (res, _, _, _) = res
