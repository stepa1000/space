{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

data SpaceOperator = SpaceOperator
  { spaceMapBool :: [Map (Int,Int) Bool]
  , spaceArea :: Int
  } deriving Generic

instance HasWaveOperator SpaceOperator where
  mapBool = the @"spaceMapBool"
  area = the @"spaceArea"

type AdjSpaceL a = Env LogLevel :.: WaveSpaceL a
type AdjSpaceR a = WaveSpaceR a :.: Reader LogLevel

instance MArray TArray SpaceOperator IO

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
  coadjLogInfoM $ fmap (const "Initial SpaceOperators") wll
  aso <- newArray_ ((0,0),(h-1,h-1))
  coadjLogInfoM $ fmap (const "Create array") wll
  mapM_
    (\ i -> do
      coadjLogDebugM $ fmap (const $ "newGenArray:area: start generate") wll
      a <- randomRIO pa
      coadjLogDebugM $ fmap (const $ "newGenArray:area:" .< a) wll
      writeArray aso i $ SpaceOperator [] a
    ) $ range ((0,0),(h-1,h-1))
  coadjLogInfoM $ fmap (const "initial Operands") wll
  oper <- initialOperandIO h
  return $ void $ ((createCoadj $ Identity aso) @## (createCoadj $ Identity oper)) @## wll

initSpaceOperator :: W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity () -> IO ()
initSpaceOperator = initWaveOperator (return . (`div` 2)) . getWaveSpace

iterateSpace :: W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity () -> IO ()
iterateSpace = iterateWaveOperator . getWaveSpace

drowSpace :: W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity Float -> IO Picture
drowSpace = fmap (Color white) . coadjDrowOperand . getAdjOperand . getWaveSpace

spaceRandomKeyWrite :: 
  Int -> -- length array
  Int -> -- elements
  W.AdjointT (AdjSpaceL SpaceOperator) (AdjSpaceR SpaceOperator) Identity (Int,Int) -> 
  IO Key
spaceRandomKeyWrite h ikey = coadjRandomKeyWrite h ikey . getAdjOperand . getWaveSpace
