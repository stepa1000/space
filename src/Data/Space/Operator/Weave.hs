{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

module Data.Space.Operator.Weave where

import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Class
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TArray
import Control.Core.Composition
import Control.Base.Comonad
import Control.Core.Biparam
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad
import Data.Functor.Adjunction
import GHC.Generics
import GHC.Float
import Data.Array.MArray
import Data.Space.Operand
import Data.Space.Operator
import Data.Space.Operator.Wave
import Control.Lens
import Control.Base.Comonad
import System.Random
import Control.Exception
import Control.Concurrent.Async
import Data.Map
import Data.Maybe
import Data.Foldable
import Data.Logger

class HasRActive a where
  chance :: Lens' a (Int,Int)
  amountWeave :: Lens' a Int

araundActive :: (Comonad w, MArray TArray a IO, HasWaveOperator a) =>
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> 
  IO [(Int,Int)]
araundActive w = do
  let operT = getOperator w
  ixT <- getBounds operT
  operD <- getOperand w
  lmj <- forM (range ixT) (\i->do
    operatorI <- readArray operT i
    let arreaOpe = operatorI^.area
    let lk = Prelude.filter (inRange ixT)  $ range $ sectorBox i arreaOpe
    lb <- forM lk (\k->readArray operD k)
    if or lb then Just k else Nothing
    ) 
  return $ catMaybes lmj

memorizeWeave :: (Comonad w, MArray TArray a IO, HasWaveOperator a, HasRActive a) =>
  (Int,Int) ->
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> 
  IO ()
memorizeWeave p w = do
  let operT = getOperator w
  ixT <- getBounds operT
  operD <- getOperand w
  operatorI <- readArray operT i
  let arreaOpe = operatorI^.area
  let lk = Prelude.filter (inRange ixT) $ range $ sectorBox i arreaOpe
  lmb <- forM lk (\k-> do
    b <- readArray operD k
    return $ if b then Just (k,b) else Nothing
    )
  let m = mconcat $ fmap (\(k,b)-> singleton k b) $ join $ catMaybes lmb
  writeArray operT i (over mapBool (m:) operatorN)
  
setChanceKeyForActive :: (Comonad w, MArray TArray a IO, HasWaveOperator a) =>
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> 
  IO ()
setChanceKeyForActive w = do
  let operT = getOperator w
  ixT <- getBounds operT
  operD <- getOperand w
  lk <- araundActive w
  forM_ lk (\k->do
    operatorK <- readArray operT k
    let (ch,mch) = operatorK^.chance
    let amw = operatorK^.amountWeave
    let lm = operatorK^.mapBool
    if length lm < amv then do
        mn <- randomRIO (0,mch)
        if mn <= ch then do
            memorizeWeave k w
          else return ()
      else return ()
    )
