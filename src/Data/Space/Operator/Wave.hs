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

class HasWavwOperator a where
  listBool :: Lens' a [[Bool]]
  area :: Lens' a Int
  
type WaveSpaceL a = Env Operand :.: Env (Operator a)
type WaveSpaceR a = Reader (Operator a) :.: Reader Operand

idIxWaveSpace :: (Comonad w, MArray TArray a IO) => W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w a -> IO Bool
idIxWaveSpace w = do
  let operD = coask $ coadjFst w
  let operT = coask $ coadjSnd w
  ixD <- getBounds operD
  ixT <- getBounds operT
  return $ ixD == ixT
  
initWaveOperator :: (Comonad w, MArray TArray a IO, HasWavwOperator a) => 
  W.AdjointT (WaveSpaceL a) (WaveSpaceR a) w a -> IO ()
initWaveOperator w = do
  bIx <- idIxWaveSpace w
  if bIx
    then do
      let operD = coask $ coadjFst w
      ixD <- getBounds operD
      mapM_ (\i -> initWaveOperator' i w) ixD
    else throwIO $ ErrorCall "idIxWaveSpace: not eq"
  where
    initWaveOperator' i w = do 
      let operD = coask $ coadjFst w
      let operT = coask $ coadjSnd w
      operator <- readArray operT i
      lb <- atomically $ getSectorList operD i (operator^.area)
      let lengthLBool = length lb
      let combi = 2^lengthLBool
      let halfCombi = combi/2
      mapM_ (\_-> do
        rlBool <- mapM (const randomIO) [0..lengthLBool]
        operatorN <- readArray operT i
        writeArray operT i (over listBool (rlBool:) operatorN)
        ) [0..halfCombi]
      
  
