{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Musicology.GramsStreamly where

import           Musicology.Internal.Helpers    ( release )
import qualified Data.Map                      as M
import qualified Data.List                     as L
import           Safe.Foldable                  ( minimumMay )

import           Streamly
import           Streamly.Prelude               ( (|:)
                                                , nil
                                                )
import qualified Streamly.Prelude              as S

import           Control.Concurrent
import           Control.Monad                  ( forever
                                                , filterM
                                                )
-- import           Control.Monad.Trans            ( lift )

delay n = S.yieldM $ do
  threadDelay (n * 1000000)
  tid <- myThreadId
  putStrLn (show tid ++ ": Delay " ++ show n)

p n = threadDelay (n * 1000000) >> return n

skipgramsR
  :: forall k n m t a
   . (Num k, Ord k, Integral n, Monad m, IsStream t)
  => m Bool
  -> ([a] -> Bool)
  -> (a -> a -> k)
  -> n
  -> k
  -> t m a
  -> t m [a]
skipgramsR coin pred cost n k input = S.concatMap (S.fromList . fst) scanned
 where
  scanned = S.postscanlM' newGrams ([], []) input
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
  :: forall n k m t a
   . (Num k, Ord k, Integral n, MonadAsync m, IsStream t)
  => m Bool
  -> ([a] -> Bool)
  -> (a -> a -> k)
  -> n
  -> k
  -> t m a
  -> t m [a]
skipgramsRS coin pred cost n k input =
  S.concatMap (S.concatMap S.fromList) $ S.unfoldrM unconsItem $ Just
    ([], 0, M.empty, adapt input)
 where
  unconsItem
    :: Maybe ([(n, k, [a], Int)], Int, M.Map Int [[a]], SerialT m a)
    -> m
         ( Maybe
             ( t m [[a]]
             , Maybe ([(n, k, [a], Int)], Int, M.Map Int [[a]], SerialT m a)
             )
         )
  unconsItem Nothing                        = return Nothing
  unconsItem (Just (pfxs, i, queue, items)) = do
    uc <- (S.uncons items)
    case uc of
      Nothing -> return $ Just (S.fromList $ M.elems queue, Nothing)
      Just (candidate, rest) -> do
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
        return $ Just (S.fromList rels, Just (pfxs', i + 1, queue', rest))
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
