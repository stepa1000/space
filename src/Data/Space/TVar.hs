{-# LANGUAGE TypeOperators #-}

module Data.Space.TVar where

-- import Data.Space.Base

import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Core.Composition
import Control.Monad.Co
import Control.Monad.Free
import Control.Monad.Reader
import Data.Functor.Adjunction
import Data.Universe
import GHC.Generics

-- type Universe2TVar a =

makeUniverse2TVar ::
  a ->
  Int ->
  IO (Universe2 (Free V1 (TVar a)))
makeUniverse2TVar a i = do
  ta <- newTVarIO a
  (fmap . fmap) (g . fmap fst) $ makeUniverse2M f (ta, i)
  where
    g (Just a) = Pure a
    g Nothing = Free undefined
    f (_, i) | i > 0 = do
      ta <- newTVarIO a
      return $ Just (ta, i - 1)
    f (_, _) = return Nothing

utoUEnv :: Universe2 (Free V1 a) -> Universe2 (((Free V1) :.: (Env a)) ())
utoUEnv = fmap Comp1 . (fmap . fmap) (\a -> env a ())

makeUniverse2EnvTVar ::
  a ->
  Int ->
  IO (Universe2 ((Free V1 :.: Env (TVar a)) ()))
makeUniverse2EnvTVar a i = utoUEnv <$> makeUniverse2TVar a i

extractU :: (Adjunction f g, Comonad w) => w (f a) -> a
extractU = extractL . extract

extractFst :: (Adjunction f g, Adjunction f2 g2, Comonad w) => w ((f2 :.: f) a) -> f a
extractFst = extractL . unComp1 . extract

extractSnd :: (Adjunction f g, Adjunction f2 g2, Comonad w) => w ((f2 :.: f) a) -> f2 a
extractSnd = fmap extractL . unComp1 . extract

getUTVar' :: Universe2 ((Free V1 :.: Env (TVar a)) b) -> TVar a
getUTVar' = fst . runEnv . extractL . unComp1 . extract

readUTVar :: Universe2 ((Free V1 :.: Env (TVar a)) b) -> STM a
readUTVar = readTVar . getUTVar'

swapUTVar :: Universe2 ((Free V1 :.: Env (TVar a)) a) -> STM a
swapUTVar w = swapTVar (getUTVar' w) (extractU w)

readRangeUTVar :: (Int, Int) -> (Int, Int) -> Universe2 ((Free V1 :.: Env (TVar a)) a) -> STM [[a]]
readRangeUTVar px py = sequence . fmap sequence . (fmap . fmap) readTVar . takeRange2 px py . extend getUTVar'

compU :: (Adjunction f g, Adjunction f2 g2) => Universe2 (f a) -> Universe2 (f2 b) -> Universe2 ((f2 :.: f) (a, b))
compU u1 u2 = fmap (\(x, y) -> Comp1 $ fmap (\a -> fmap (\b -> (b, a)) x) y) $ zipU2 u1 u2

{-
initUniverse2TVar :: a -> IO (Universe2 (Maybe (TVar a)))
initUniverse2TVar a = do
  where
    listTVar = repeat ()
-}
