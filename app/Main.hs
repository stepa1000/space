{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

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
import Graphics.Gloss.Interface.IO.Game
import Data.Space
import Graphics.Gloss.Interface.IO.Interact
import Data.CoAndKleisli
import Data.Logger

main :: IO ()
main = do
  w <- initEmptySpace Debug (1,2) 100
  playIO 
    (InWindow "WaveSpace" (500,500) (0,0))
    black
    1
    w
    (fmap (Translate (-50) (-50)) . drowSpace . fmap (const 1))
    eventSpace
    (const return)
  where
    eventSpace (EventKey (SpecialKey KeySpace) Down _ _) a = flipWM $! extend iterateSpace a
    eventSpace (EventKey (Char 'r') Down _ _) a = flipWM $! extend initSpaceOperatorKey a 
    eventSpace (EventKey (SpecialKey KeyEnter) Down _ _) a = fmap void $! flipWM $! extend (spaceRandomKeyWrite 2 12) $! fmap (const (5,5)) a
    eventSpace _ a = return a
  
