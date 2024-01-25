{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

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

data SpaceOperator = SpaceOperator
  { spaceMapBool :: [Map (Int,Int) Bool]
  , spaceArea :: Int
  } deriving Generic

instance HasWavwOperator SpaceOperator where
  mapBool = the @"spaceMapBool"
  area = the @"spaceArea"

initEmptySpace :: MArray TArray SpaceOperator IO => (Int,Int) -> Int -> IO (W.AdjointT (WaveSpaceL SpaceOperator) (WaveSpaceR SpaceOperator) Identity ())
initEmptySpace pa h = do
  aso <- newGenArray ((0,0),(h-1,h-1)) 
    (\ _ -> do
      a <- randomRIO pa
      return $ SpaceOperator [] a
    )
  oper <- initialOperandIO h
  return $ void $ (createCoadj $ Identity aso) @## (createCoadj $ Identity oper)

initSpaceOperator :: MArray TArray SpaceOperator IO => W.AdjointT (WaveSpaceL SpaceOperator) (WaveSpaceR SpaceOperator) Identity () -> IO ()
initSpaceOperator = initWaveOperator (return . (`div` 2))

iterateSpace :: MArray TArray SpaceOperator IO => W.AdjointT (WaveSpaceL SpaceOperator) (WaveSpaceR SpaceOperator) Identity () -> IO ()
iterateSpace = iterateWaveOperator

drowSpace :: MArray TArray SpaceOperator IO => W.AdjointT (WaveSpaceL SpaceOperator) (WaveSpaceR SpaceOperator) Identity Float -> IO Picture
drowSpace = fmap (Color white) . coadjDrowOperand . getAdjOperand

spaceRandomKeyWrite :: MArray TArray SpaceOperator IO => 
  Int -> 
  Int -> 
  W.AdjointT (WaveSpaceL SpaceOperator) (WaveSpaceR SpaceOperator) Identity (Int,Int) -> 
  IO Key
spaceRandomKeyWrite h ikey = coadjRandomKeyWrite h ikey . getAdjOperand
