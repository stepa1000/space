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
  
type WaveSpaceL a = Env (TVar Operand) :.: Env (Operator a)
type WaveSpaceR a = Reader (Operator a) :.: Reader (TVar Operand)

getOperand :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO Operand
getOperand = fmap coask . getAdjOperand 

getOperator :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> Operator a
getOperator = coask . getAdjOperator
{-
class HaveWaveSpace fl fr where
  getAdjOperand :: Comonad w => W.AdjointT (fl a) (fr a) w b -> W.AdjointT (Env Operand) (Reader Operand) w b
  getAdjOperator :: Comonad w => W.AdjointT (fl a) (fr a) w b -> W.AdjointT (Env (Operator a)) (Reader (Operator a)) w b
-}
getAdjOperand :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO (W.AdjointT (Env Operand) (Reader Operand) w b)
getAdjOperand w = do
  let wtv = coask $ snd $ unCompSysAdjComonad w
  ope <- readTVarIO wtv
  return $ fmap (const $ extract w) $ createCoadj $ fmap (const ope) $ lower w

setAdjOperand :: Comonad w => Operand -> W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO ()
setAdjOperand o w = do
  let wtv = coask $ snd $ unCompSysAdjComonad w
  atomically $ writeTVar wtv o

newAdjOperand :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO ()
newAdjOperand w = do
  operD <- getOperand w
  ixD <- getBounds operD
  a <- newArray ixD False
  setAdjOperand a w

getAdjOperator :: Comonad w => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> W.AdjointT (Env (Operator a)) (Reader (Operator a)) w b
getAdjOperator = fst . unCompSysAdjComonad

idIxWaveSpace :: (Comonad w, MArray TArray a IO) => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO Bool
idIxWaveSpace w = do
  operD <- getOperand w
  let operT = getOperator w
  ixD <- getBounds operD
  ixT <- getBounds operT
  return $ ixD == ixT

initWaveOperatorKeyOne :: (Comonad w, MArray TArray a IO, HasWaveOperator a) =>
  W.AdjointT (Env LogLevel) (Reader LogLevel) w b2 -> 
  (Int,Int) -> -- point
  (Int,Int) -> -- key
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> 
  IO ()
initWaveOperatorKeyOne wl pp pk w = do
  bIx <- idIxWaveSpace w
  operD <- getOperand w
  ixD <- getBounds operD
  let operT = getOperator w
  if and [bIx, inRange ixD pp, inRange ixD pk]
    then do
      let m = singleton pk True
      operatorN <- readArray operT pp
      writeArray operT pp (over mapBool (m:) operatorN)
    else do
      coadjLogErrorM "initWaveOperatorKeyOne: if False" wl
      throwIO $ ErrorCall "initWaveOperatorKeyOne: if False"

initWaveOperatorKeyList (Comonad w, MArray TArray a IO, HasWaveOperator a) =>
  W.AdjointT (Env LogLevel) (Reader LogLevel) w b2 -> 
  (Int,Int) -> -- point
  [(Int,Int)] -> -- key
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> 
  IO ()
initWaveOperatorKeyList wl pp pk w = do
  bIx <- idIxWaveSpace w
  operD <- getOperand w
  ixD <- getBounds operD
  let operT = getOperator w
  if and [bIx, inRange ixD pp, inRange ixD pk]
    then do
      let m = mconcat $ fmap (\i-> singleton i) True pk
      operatorN <- readArray operT pp
      writeArray operT pp (over mapBool (m:) operatorN)
    else do
      coadjLogErrorM "initWaveOperatorKeyOne: if False" wl
      throwIO $ ErrorCall "initWaveOperatorKeyOne: if False"

initWaveOperatorKey :: (Comonad w, MArray TArray a IO, HasWaveOperator a) => 
  W.AdjointT (Env LogLevel) (Reader LogLevel) w b2 ->
  (Int -> IO Int) -> -- number of length keys
  (Int -> IO Int) -> -- number of map
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO ()
initWaveOperatorKey wl f f2 w = do
  bIx <- idIxWaveSpace w
  if bIx
    then do
      coadjLogDebugM "initWaveOperatorKey:start" wl
      operD <- getOperand w
      ixD <- getBounds operD
      mapM_ (\i -> initWaveOperator' wl i w) $ range ixD
    else do
      coadjLogErrorM "idIxWaveSpace: not eq" wl
      throwIO $ ErrorCall "idIxWaveSpace: not eq"
  where
    initWaveOperator' wl i w = do 
      operD <- getOperand w
      let operT = getOperator w
      ixD <- getBounds operD
      operator <- readArray operT i
      lb <- atomically $ getSectorList operD i (operator^.area)
      let lengthLBool = length lb
      fi <- f2 lengthLBool
      forM_ [0..fi] (\j-> do
        coadjLogDebugM ("initWaveOperatorKey:lengthLBool:" .< lengthLBool) wl
        coadjLogDebugM ("initWaveOperatorKey:IndexForList:" .< j) wl
        h <- f lengthLBool
        let h2 = if h <= 0 then 1 else h
        coadjLogDebugM ("initWaveOperatorKey:functionForBoxArea:" .< h2) wl
        let rlBool = fmap (const True) [0.. h2]
        let pi = Prelude.filter (inRange ixD) $ range $ sectorBox i (operator^.area)
        li <- mapM (const (randomInList wl pi)) [0.. h2]
        let rmapBool = mconcat $ Prelude.map (\(x,y)-> singleton x y) $ zip li rlBool
        operatorN <- readArray operT i
        coadjLogDebugM "initWaveOperatorKey:operatorN:readArray:OK" wl
        writeArray operT i (over mapBool (rmapBool:) operatorN)
        coadjLogDebugM "initWaveOperatorKey:operatorN:writeArray:OK" wl
        )
    randomInList wl l = do
      let lenList = length l
      coadjLogDebugM ("initWaveOperatorKey:randomInList:length" .< lenList) wl
      j <- randomRIO (0,lenList - 1)
      return $ l !! j

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
  operD <- getOperand w
  let operT = getOperator w
  ixD <- getBounds operD
  newAdjOperand w
  operD2 <- getOperand w
  mapConcurrently_ (\ i -> do -- mapConcurrently_
    atomically $ do
      operator <- readArray operT i
      let lPatternBoll = operator^.mapBool
      b <- foldlM (\b0 p -> do
        let lk = keys p
        bx <- mapM (\k->do
          let (Just bk) = Data.Map.lookup k p
          bd <- readArray operD k
          coadjLogDebugM ("iterateWaveOperator:readArray:Value:" .< bd) wl
          coadjLogDebugM ("iterateWaveOperator:readArray:MapIndex:" .< k) wl
          coadjLogDebugM ("iterateWaveOperator:readArray:MapValue:" .< bk) wl
          return $ bd == bk
          ) lk
        when (and bx) $ do
          coadjLogDebugM ("iterateWaveOperator:lPatternBoll:" .< (p, i)) wl
        let bx2 = if p == empty then False else (and bx)
        return $ b0 || bx2
        ) False lPatternBoll
      writeArray operD2 i b
    ) $ range ixD
