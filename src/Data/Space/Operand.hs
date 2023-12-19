{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Space.Operand where

import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TArray
import Control.Core.Composition
import Control.Core.Biparam
import Control.Monad.Reader
import Data.Functor.Adjunction
import GHC.Generics
import Data.Array.MArray
import Data.Array
import Data.Ix

instance MArray TArray Bool IO

type Operand = TArray (Int,Int) Bool

initialOperandIO :: Int -> IO Operand
initialOperandIO i = newArray ((0,0),(i-1,i-1)) False

getSectorList :: Operand -> (Int,Int) -> Int -> STM [Bool]
getSectorList ops (xi,yi) h = do
  bounds <- getBounds ops
  let xi1 = xi - h
  let xi2 = xi + h
  let yi1 = yi - h
  let yi2 = yi + h
  let li = filter (inRange bounds) $ range ((xi1,yi1),(xi2,yi2))
  mapM (\i-> readArray ops i) li
  
-- Adjoint

coadjGetSectorList :: Comonad w => Int -> W.AdjointT (Env Operand) (Reader Operand) w (Int,Int) -> STM [Bool]
coadjGetSectorList h = coadjBiparam (\ops i-> getSectorList ops i h)
