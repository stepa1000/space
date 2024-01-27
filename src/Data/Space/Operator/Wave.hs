{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}

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
import Control.Monad
import Data.Functor.Adjunction
import GHC.Generics
import GHC.Float
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
import Data.Logger

class HasWaveOperator a where
  mapBool :: Lens' a [Map (Int,Int) Bool]
  area :: Lens' a Int
  
type WaveSpaceL a = Env Operand :.: Env (Operator a)
type WaveSpaceR a = Reader (Operator a) :.: Reader Operand 

getOperand :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> Operand
getOperand = coask . getAdjOperand 

getOperator :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> Operator a
getOperator = coask . getAdjOperator
{-
class HaveWaveSpace fl fr where
  getAdjOperand :: Comonad w => W.AdjointT (fl a) (fr a) w b -> W.AdjointT (Env Operand) (Reader Operand) w b
  getAdjOperator :: Comonad w => W.AdjointT (fl a) (fr a) w b -> W.AdjointT (Env (Operator a)) (Reader (Operator a)) w b
-}
getAdjOperand :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> W.AdjointT (Env Operand) (Reader Operand) w b
getAdjOperand = snd . unCompSysAdjComonad

getAdjOperator :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> W.AdjointT (Env (Operator a)) (Reader (Operator a)) w b
getAdjOperator = fst . unCompSysAdjComonad

idIxWaveSpace :: (Comonad w, MArray TArray a IO) => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO Bool
idIxWaveSpace w = do
  let operD = getOperand w
  let operT = getOperator w
  ixD <- getBounds operD
  ixT <- getBounds operT
  return $ ixD == ixT

initWaveOperatorKey :: (Comonad w, MArray TArray a IO, HasWaveOperator a) => 
  W.AdjointT (Env LogLevel) (Reader LogLevel) w b2 ->
  (Int -> IO Int) ->
  (Int -> IO Int) ->
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO ()
initWaveOperatorKey wl f f2 w = do
  bIx <- idIxWaveSpace w
  if bIx
    then do
      coadjLogDebugM "initWaveOperatorKey:start" wl
      let operD = getOperand w
      ixD <- getBounds operD
      mapM_ (\i -> initWaveOperator' wl i w) $ range ixD
    else do
      coadjLogErrorM "idIxWaveSpace: not eq" wl
      throwIO $ ErrorCall "idIxWaveSpace: not eq"
  where
    initWaveOperator' wl i w = do 
      let operD = getOperand w
      let operT = getOperator w
      ixD <- getBounds operD
      operator <- readArray operT i
      lb <- atomically $ getSectorList operD i (operator^.area)
      let lengthLBool = length lb
      fi <- f2 lengthLBool
      forM_ [0..fi] (\j-> do
        coadjLogDebugM ("initWaveOperatorKey:lengthLBool:" .< lengthLBool) wl
        coadjLogDebugM ("initWaveOperatorKey:IndexForList:" .< j) wl
        h2 <- f lengthLBool
        coadjLogDebugM ("initWaveOperatorKey:functionForBoxArea:" .< h2) wl
        let rlBool = fmap (const True) [0.. h2]
        let pi = sectorBox i (operator^.area)
        lri <- mapM (const (randomRIO pi)) [0.. h2]
        let li = Prelude.filter (inRange ixD) lri 
        let rmapBool = mconcat $ Prelude.map (\(x,y)-> singleton x y) $ zip li rlBool
        operatorN <- readArray operT i
        coadjLogDebugM "initWaveOperatorKey:operatorN:readArray:OK" wl
        writeArray operT i (over mapBool (rmapBool:) operatorN)
        coadjLogDebugM "initWaveOperatorKey:operatorN:writeArray:OK" wl
        )

clearWaveOperatorKey :: (Comonad w, MArray TArray a IO, HasWaveOperator a) => 
  W.AdjointT (Env LogLevel) (Reader LogLevel) w b2 ->
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> 
  IO ()
clearWaveOperatorKey wl w = do
  let operT = getOperator w
  ixT <- getBounds operT
  coadjLogDebugM ("clearWaveOperatorKey:bound:" .< ixT) wl
  coadjLogDebugM "clearWaveOperatorKey:mapM_:start" wl
  mapM_ (\i -> do
    coadjLogDebugM ("clearWaveOperatorKey:mapM_:index:" .< i) wl
    operatorN <- readArray operT i
    coadjLogDebugM "clearWaveOperatorKey:readArray:OK" wl
    writeArray operT i (set mapBool [] operatorN)
    coadjLogDebugM "clearWaveOperatorKey:writeArray:OK" wl
    ) $ range ixT
  coadjLogDebugM "clearWaveOperatorKey:mapM_:end" wl

iterateWaveOperator :: (Comonad w, MArray TArray a IO, HasWaveOperator a) => 
  W.AdjointT (Env LogLevel) (Reader LogLevel) w b2 ->
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO ()
iterateWaveOperator wl w = do
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
      when b $ do
        coadjLogDebugM ("iterateWaveOperator:lPatternBoll:" .< lPatternBoll) wl
      writeArray operD i b
    ) $ range ixD
