{-# LANGUAGE TypeOperators #-}

module Data.Space.Base where

import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Core.Composition
import Data.Functor.Adjunction
import Data.Universe

type Space f g a = W.AdjointT f g Universe2 a

makeSpace :: (Monad m, Functor f) => f () -> g b -> g b -> a -> (a -> m (Maybe (g b, a))) -> m (Space f g b)
makeSpace fa fgNil gCentr a mg = do
  ll <- fixF a
  rl <- fixF a
  ll' <- fixF a
  rl' <- fixF a
  llu <- mapM genU ll'
  lru <- mapM genU rl'
  return $ W.AdjointT $ (const $ Universe2 $ Universe llu (Universe ll gCentr rl) lru) <$> fa
  where
    genU g = do
      ll <- fixF a
      rl <- fixF a
      return $ Universe ll g rl
    fixF a = do
      mgb <- mg a
      case mgb of
        (Just (gb, a2)) -> do
          gbl <- fixF a2
          return $ gb : gbl
        Nothing -> return $ repeat fgNil

upS :: (Adjunction f g) => Space f g a -> Space f g a
upS = hoistWAdj up

downS :: (Adjunction f g) => Space f g a -> Space f g a
downS = hoistWAdj down

leftS :: (Adjunction f g) => Space f g a -> Space f g a
leftS = hoistWAdj left2

rightS :: (Adjunction f g) => Space f g a -> Space f g a
rightS = hoistWAdj right2

takeRange2S :: (Adjunction f g) => (Int, Int) -> (Int, Int) -> Space f g a -> [[a]]
takeRange2S px py u = takeRange2 px py $ lower u
