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

araundActive :: (Comonad w, MArray TArray a IO, HasWaveOperator a) =>
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w b -> IO [(Int,Int)]
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


