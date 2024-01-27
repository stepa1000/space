{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.Space where

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
import Data.Space.Operator.Wave
import Data.Space.Operand
import Data.Space.Operator
import Data.Map
import Control.Core.Biparam
import Data.Functor.Identity
import Data.Functor
import Data.Generics.Product.Any
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.Logger
import Control.Exception

data SpaceOperator = SpaceOperator
  { spaceMapBool :: [Map (Int,Int) Bool]
  , spaceArea :: Int
  } deriving Generic

instance HasWaveOperator SpaceOperator where
  mapBool = the @"spaceMapBool"
  area = the @"spaceArea"

type AdjSpaceL a = Env LogLevel :.: WaveSpaceL a
type AdjSpaceR a = WaveSpaceR a :.: Reader LogLevel

{-IO where
  getBounds (TArray l u _ _) = return (l, u)
  getNumElements (TArray _ _ n _) = return n
  newArray b e = IO $ newTArray# b e
  unsafeRead (TArray _ _ _ arr# ) (I# i# ) = case (indexArray# arr# i# ) of
    (# tvar# #) -> readTVarIO (TVar tvar#)
  unsafeWrite (TArray _ _ _ arr# ) (I# i# ) e = case indexArray# arr# i# of
    (# tvar# #) -> atomically $ writeTVar (TVar tvar# ) e
-}

{-
class HaveAdjSpace fl fr where
  getLogLevel :: Comonad w => W.AdjointT (fl a) (fr a) w b -> W.AdjointT (Env LogLevel) (Reader LogLevel) w b
  getWaveSpace :: Comonad w => W.AdjointT (fl a) (fr a) w b -> W.AdjointT (WaveSpaceR a) (WaveSpaceL a) w b
-}
getLogLevel :: Comonad w => W.AdjointT (AdjSpaceL a) (AdjSpaceR a) w b -> W.AdjointT (Env LogLevel) (Reader LogLevel) w b
getLogLevel = snd . unCompSysAdjComonad

getWaveSpace :: Comonad w => W.AdjointT (AdjSpaceL a) (AdjSpaceR a) w b -> W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b
getWaveSpace = fst . unCompSysAdjComonad

initEmptySpace :: 
  LogLevel -> 
  (Int,Int) -> 
  Int -> 
  IO (W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity ())
initEmptySpace ll pa h = do
  let wll = createCoadj $ Identity ll
  coadjLogInfoM "Initial SpaceOperators" wll
  easo <- try @SomeException $ newArray_ ((0,0),(h-1,h-1))
  case easo of
    (Right aso) -> do
      coadjLogInfoM "Create array" wll
      mapM_
        (\ i -> do
          coadjLogDebugM "newGenArray:area: start generate" wll
          a <- randomRIO pa
          coadjLogDebugM ("newGenArray:area:" .< a) wll
          writeArray aso i $ SpaceOperator [] a
        ) $ range ((0,0),(h-1,h-1))
      coadjLogInfoM "initial Operands" wll
      oper <- initialOperandIO h
      return $ void $ ((createCoadj $ Identity aso) @## (createCoadj $ Identity oper)) @## wll
    (Left e) -> error $ "initEmptySpace:" .< e

initSpaceOperatorKey :: W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity () -> IO ()
initSpaceOperatorKey w = do
  coadjLogInfoM "initSpaceOperator:call" $ getLogLevel w
  clearWaveOperatorKey (getLogLevel w) (getWaveSpace w)
  coadjLogDebugM "initSpaceOperator:clear" $ getLogLevel w
  (initWaveOperatorKey (getLogLevel w) (return . (`div` 2)) (return . const 20) . getWaveSpace) w
  coadjLogDebugM "initSpaceOperator:end" $ getLogLevel w

iterateSpace :: W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity () -> IO ()
iterateSpace w = do
  coadjLogInfoM "iterateSpace:call" $ getLogLevel w
  (iterateWaveOperator . getWaveSpace) w

drowSpace :: W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity Float -> IO Picture
drowSpace = fmap (Color white) . coadjDrowOperand . getAdjOperand . getWaveSpace

spaceRandomKeyWrite :: 
  Int -> -- length array
  Int -> -- elements
  W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity (Int,Int) -> 
  IO Key
spaceRandomKeyWrite h ikey w = do
  coadjLogInfoM "spaceRandomKeyWrite:call" $ getLogLevel w
  (coadjRandomKeyWrite h ikey . getAdjOperand . getWaveSpace) w
