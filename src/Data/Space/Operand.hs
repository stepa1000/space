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
import Control.Monad
import Data.Functor.Adjunction
import GHC.Generics
import Data.Array.MArray
import Data.Array
import Data.Ix
import Graphics.Gloss.Data.Picture
import GHC.Float
import System.Random

type Operand = TArray (Int,Int) Bool

initialOperandIO :: Int -> IO Operand
initialOperandIO i = newArray ((0,0),(i-1,i-1)) False

sectorBox :: (Int,Int) -> Int -> ((Int,Int),(Int,Int))
sectorBox (xi,yi) h = let
  xi1 = xi - h
  xi2 = xi + h
  yi1 = yi - h
  yi2 = yi + h
  in ((xi1,yi1),(xi2,yi2))

boxS :: ((Int,Int),(Int,Int)) -> Int
boxS ((x1,y1),(x2,y2)) = l1*l2 
  where
    l1 = abs (x1-x2)
    l2 = abs (y1-y2)

getSectorList :: Operand -> (Int,Int) -> Int -> STM [Bool]
getSectorList ops (xi,yi) h = do
  bounds <- getBounds ops
  let ((xi1,yi1),(xi2,yi2)) = sectorBox (xi,yi) h
  let li = filter (inRange bounds) $ range ((xi1,yi1),(xi2,yi2))
  mapM (\i-> readArray ops i) li

subSectors :: Int -> (Int,Int) -> Int -> [((Int,Int),(Int,Int))]
subSectors c (xi,yi) h = let
  xi1 = xi - h
  xi2 = xi + h
  yi1 = yi - h
  yi2 = yi + h
  lx = [xi1..xi2]
  ly = [yi1..yi2]
  in do
    x1 <- lx
    x2 <- lx
    y1 <- ly
    y2 <- ly
    guard $ abs (x1-x2) > c
    guard $ abs (y1-y2) > c
    guard (x1<=x2)
    guard (y1<=y2)
    return ((x1,y1),(x2,y2))

type Key = [((Int,Int),Bool)]

getSectorListWithKey :: Operand -> (Int,Int) -> Int -> STM Key
getSectorListWithKey ops (xi,yi) h = do
  bounds <- getBounds ops
  let ((xi1,yi1),(xi2,yi2)) = sectorBox (xi,yi) h
  let li = filter (inRange bounds) $ range ((xi1,yi1),(xi2,yi2))
  mapM (\i-> do
    b <- readArray ops i
    return (i,b)
    ) li

oneWriteToOperand :: Operand -> (Int,Int) -> IO ()
oneWriteToOperand ope i = do
  bounds <- getBounds ope
  if inRange bounds i
    then writeArray ope i True
    else error "oneWriteToOperand:inRange:False"

oneReadToOperand :: Operand -> (Int,Int) -> IO Bool
oneReadToOperand ope i = do
  bounds <- getBounds ope
  if inRange bounds i
    then readArray ope i
    else error "oneWriteToOperand:inRange:False"

randomKeyWrite :: Operand -> (Int,Int) -> Int -> Int -> IO Key
randomKeyWrite ope (xi,yi) h ikey = do
  bounds <- getBounds ope
  let ikey2 = if ikey > (h*2)^2 then (h*2)^2 else ikey
  let rlBool = fmap (const True) [0.. ikey2]
  let pi = sectorBox (xi,yi) h
  lri <- mapM (const (randomRIO pi)) [0..ikey2]
  let li = Prelude.filter (inRange bounds) lri 
  mapM (\(i,b)->do
      writeArray ope i b
      return (i,b)
    ) $ zip li rlBool

randomKeySector :: Operand -> ((Int,Int),(Int,Int)) -> Int -> IO Key
randomKeySector ope ((x1,y1),(x2,y2)) h = do
  bounds <- getBounds ope
  let rlBool = fmap (const True) [0.. h]
  lrx <- mapM (const (randomRIO (x1,x2))) [0..h]
  lry <- mapM (const (randomRIO (y1,y2))) [0..h]
  let li = Prelude.filter (inRange bounds) $ zip lrx lry
  return $! zip li rlBool

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

clearOperand :: Operand -> IO ()
clearOperand ope = do
  bounds <- getBounds ope
  mapM_ (\i-> writeArray ope i False
    ) $ range bounds

-- Adjoint

coadjGetSectorList :: Comonad w => Int -> W.AdjointT (Env Operand) (Reader Operand) w (Int,Int) -> STM [Bool]
coadjGetSectorList h = coadjBiparam (\ops i-> getSectorList ops i h)

coadjGetSectorListWithKey :: Comonad w => Int -> W.AdjointT (Env Operand) (Reader Operand) w (Int,Int) -> STM Key
coadjGetSectorListWithKey h = coadjBiparam (\ops i-> getSectorListWithKey ops i h)

coadjDrowOperand :: Comonad w => W.AdjointT (Env Operand) (Reader Operand) w Float -> IO Picture
coadjDrowOperand = coadjBiparam (\ops i-> drowOperand i ops)

coadjRandomKeyWrite :: Comonad w => Int -> Int -> W.AdjointT (Env Operand) (Reader Operand) w (Int,Int) -> IO Key
coadjRandomKeyWrite h ikey = coadjBiparam (\ops i-> randomKeyWrite ops i h ikey)

coadjClearOperand :: Comonad w => W.AdjointT (Env Operand) (Reader Operand) w b -> IO ()
coadjClearOperand = coadjBiparam (\ope _ -> clearOperand ope)

coadjOneWriteToOperand :: Comonad w => (Int,Int) -> W.AdjointT (Env Operand) (Reader Operand) w b -> IO ()
coadjOneWriteToOperand i = coadjBiparam (\ope _ -> oneWriteToOperand ope i)

coadjOneReadToOperand :: Comonad w => (Int,Int) -> W.AdjointT (Env Operand) (Reader Operand) w b -> IO Bool
coadjOneReadToOperand i = coadjBiparam (\ope _ -> oneReadToOperand ope i)

coadjRandomKeySector :: Comonad w =>
  ((Int,Int),(Int,Int)) -> 
  Int -> 
  W.AdjointT (Env Operand) (Reader Operand) w b -> IO Key
coadjRandomKeySector p i = coadjBiparam (\ope _ -> randomKeySector ope p i)
