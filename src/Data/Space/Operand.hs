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
import Data.Array as A
import Data.Ix
import Graphics.Gloss.Data.Picture
import GHC.Float
import System.Random
import Control.Base.Comonad

type Operand = Array (Int,Int) Bool

initialOperandIO :: Int -> IO Operand
initialOperandIO i = newArray ((0,0),(i-1,i-1)) False

sectorBox :: (Int,Int) -> Int -> ((Int,Int),(Int,Int))
sectorBox (xi,yi) h = let
  xi1 = xi - h
  xi2 = xi + h
  yi1 = yi - h
  yi2 = yi + h
  in ((xi1,yi1),(xi2,yi2))

type AdjOperandL = Env Operand
type AdjOperandR = Reader Operand

getSectorList :: Monad m => (Int,Int) -> Int -> M.AdjointT AdjOperandL AdjOperandR m ()
getSectorList (xi,yi) h = do
  ops <- adjGetEnv
  bounds <- getBounds ops
  let ((xi1,yi1),(xi2,yi2)) = sectorBox (xi,yi) h
  let li = filter (inRange bounds) $ range ((xi1,yi1),(xi2,yi2))
  return $ fmap (\i-> ops A.! i) li

type Key = [((Int,Int),Bool)]

getSectorListWithKey :: Monad m => (Int,Int) -> Int -> M.AdjointT AdjOperandL AdjOperandR m Key
getSectorListWithKey (xi,yi) h = do
  ops <- adjGetEnv
  bounds <- getBounds ops
  let ((xi1,yi1),(xi2,yi2)) = sectorBox (xi,yi) h
  let li = filter (inRange bounds) $ range ((xi1,yi1),(xi2,yi2))
  return $ map (\i-> do
    b <- readArray ops i
    return (i,b)
    ) li

randomKeyWrite :: (Monad m, MonadIO m) => (Int,Int) -> Int -> Int -> M.AdjointT AdjOperandL AdjOperandR m IO Key
randomKeyWrite (xi,yi) h ikey = do
  ope <- adjGetEnv
  bounds <- getBounds ope
  let ikey2 = if ikey > (h*2)^2 then (h*2)^2 else ikey
  rlBool <- mapM (const $ liftIO $ randomIO) [0.. ikey2]
  let pi = sectorBox (xi,yi) h
  lri <- mapM (const (liftIO $ randomRIO pi)) [0..ikey2]
  let li = Prelude.filter (inRange bounds) lri 
  foldl (\(i,b)->do
      writeArray ope i b
      return (i,b)
    ) $ zip li rlBool

drowOperand :: Float -> Operand -> IO Picture
drowOperand h o = do
  bounds <- getBounds o
  lp <- mapM (\i@(xi,yi)-> do
    b <- readArray o i
    if b 
      then return $ Translate (int2Float xi * h) (int2Float yi * h) $ Polygon $ [(0,0),(h,0),(h,h),(0,h),(0,0)]
      else return $ Blank
    ) $ range bounds
  return $ Pictures lp

-- Adjoint

coadjGetSectorList :: Comonad w => Int -> W.AdjointT (Env Operand) (Reader Operand) w (Int,Int) -> STM [Bool]
coadjGetSectorList h = coadjBiparam (\ops i-> getSectorList ops i h)

coadjGetSectorListWithKey :: Comonad w => Int -> W.AdjointT (Env Operand) (Reader Operand) w (Int,Int) -> STM Key
coadjGetSectorListWithKey h = coadjBiparam (\ops i-> getSectorListWithKey ops i h)

coadjDrowOperand :: Comonad w => W.AdjointT (Env Operand) (Reader Operand) w Float -> IO Picture
coadjDrowOperand = coadjBiparam (\ops i-> drowOperand i ops)

coadjRandomKeyWrite :: Comonad w => Int -> Int -> W.AdjointT (Env Operand) (Reader Operand) w (Int,Int) -> IO Key
coadjRandomKeyWrite h ikey = coadjBiparam (\ops i-> randomKeyWrite ops i h ikey)

