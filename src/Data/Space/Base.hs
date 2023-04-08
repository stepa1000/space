{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Space.Base where

import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Core.Composition
import Control.Monad.Co
import Control.Monad.Trans.Adjoint as M
import Data.Functor.Adjunction
import Data.Universe

type SpaceGlobal f g = W.AdjointT f g Universe2

makeSpaceG :: (Monad m, Functor f) => f () -> g b -> g b -> a -> (a -> m (Maybe (g b, a))) -> m (SpaceGlobal f g b)
makeSpaceG fa fgNil gCentr a mg = do
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

{-
makeSpaceDS :: (Functor f) => f () -> g b -> a -> (a -> Maybe (g b, a)) -> SpaceGlobal f g b
makeSpaceDS fu a gd gmd =
  fst <$> Universe2 $
    makeUniverse
      (const $ makeUniverse f f (gmd a))
      (const $ makeUniverse f f (gmd a))
      (makeUniverse f f (gmd a))
  where
    f (Just gmba) = gmb (snd gmba)
    f _ = Nothing
-}
upSG :: (Adjunction f g) => SpaceGlobal f g a -> SpaceGlobal f g a
upSG = hoistWAdj up

downSG :: (Adjunction f g) => SpaceGlobal f g a -> SpaceGlobal f g a
downSG = hoistWAdj down

leftSG :: (Adjunction f g) => SpaceGlobal f g a -> SpaceGlobal f g a
leftSG = hoistWAdj left2

rightSG :: (Adjunction f g) => SpaceGlobal f g a -> SpaceGlobal f g a
rightSG = hoistWAdj right2

takeRange2SG :: (Adjunction f g) => (Int, Int) -> (Int, Int) -> SpaceGlobal f g a -> [[a]]
takeRange2SG px py u = takeRange2 px py $ lower u

type Space fl gl fg gg m = M.AdjointT fl gl (CoT (SpaceGlobal fg gg) m)

hoistWCoT :: (forall r. w2 (b -> m r) -> w1 (a -> m r)) -> CoT w1 m a -> CoT w2 m b
hoistWCoT f (CoT fw) = CoT $ fw . f

-- makeSpace :: (Monad m, Functor f) => f () -> a -> (a -> (Maybe (g (m (fl b)), a))) -> Space fl gl f g m b
-- makeSpace fa fgNil gCentr a mg = do
