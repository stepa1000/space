{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts#-}

module Data.Space.Operator.Wave where

import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TArray
import Control.Core.Composition
import Control.Monad.Reader
import Control.Monad.STM
import Data.Functor.Adjunction
import GHC.Generics
import Data.Array.MArray
import Data.Space.Operand
import Data.Space.Operator
import Control.Lens
import Control.Base.Comonad
import System.Random
import Control.Exception
import Control.Concurrent.Async
import Data.Map
import Data.Foldable

class HasWavwOperator a where
  mapBool :: Lens' a [Map (Int,Int) Bool]
  area :: Lens' a Int
  
type WaveSpaceL a = Env Operand :.: Env (Operator a)
type WaveSpaceR a = Reader (Operator a) :.: Reader Operand

getOperand :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> Operand
getOperand = coask . coadjFst 

getOperator :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> Operator a
getOperator = coask . coadjSnd

getAdjOperand :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> W.AdjointT (Env Operand) (Reader Operand) w b
getAdjOperand = snd . unCompSysAdjComonad

getAdjOperator :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> W.AdjointT (Env Operand) (Reader Operand) w b
getAdjOperator = fst . unCompSysAdjComonad

idIxWaveSpace :: (Comonad w, MArray TArray a IO) => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO Bool
idIxWaveSpace w = do
  let operD = getOperand w
  let operT = getOperator w
  ixD <- getBounds operD
  ixT <- getBounds operT
  return $ ixD == ixT
  
initWaveOperator :: (Comonad w, MArray TArray a IO, HasWavwOperator a) => 
  (Int -> IO Int) ->
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO ()
initWaveOperator f w = do
  bIx <- idIxWaveSpace w
  if bIx
    then do
      let operD = getOperand w
      ixD <- getBounds operD
      mapM_ (\i -> initWaveOperator' i w) $ range ixD
    else throwIO $ ErrorCall "idIxWaveSpace: not eq"
  where
    initWaveOperator' i w = do 
      let operD = getOperand w
      let operT = getOperator w
      ixD <- getBounds operD
      operator <- readArray operT i
      lb <- atomically $ getSectorList operD i (operator^.area)
      let lengthLBool = length lb
      let combi = 2^lengthLBool
      let halfCombi = combi/2
      mapM_ (\_-> do
        h2 <- f lengthLBool
        rlBool <- mapM (const randomIO) [0.. h2]
        let pi = sectorBox i (operator^.area)
        lri <- mapM (const (randomRIO pi)) [0.. h2]
        let li = Prelude.filter (inRange ixD) lri 
        let rmapBool = mconcat $ Prelude.map (\(x,y)-> singleton x y) $ zip li rlBool
        operatorN <- readArray operT i
        writeArray operT i (over mapBool (rmapBool:) operatorN)
        ) [0..halfCombi]

iterateWaveOperator :: (Comonad w, MArray TArray a IO, HasWavwOperator a) => 
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO ()
iterateWaveOperator w = do
  let operD = getOperand w
  let operT = getOperator w
  ixD <- getBounds operD
  mapConcurrently_ (\ i -> do
    atomically $ do
      operator <- readArray operT i
      let lPatternBoll = operator^.mapBool
      b <- foldlM (\b0 p -> do
        let lk = keys p
        bx <- mapM (\k->do
          let (Just bk) = Data.Map.lookup k p
          bd <- readArray operD i
          return $ bd == bk
          ) lk
        return $ b0 || (and bx)
        ) False lPatternBoll
      writeArray operD i b
    ) $ range ixD
  
