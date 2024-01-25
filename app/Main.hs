{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

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

main :: IO ()
main = playIO 
  (InWindow "WaveSpace" (500,500) (0,0))
  black
  1
  (initEmptySpace (2,5) 1000)
  (drowSpace . fmap (const 50))
  eventSpace
  (return . (const id))
  where
    eventSpace (EventKey (SpecialKey KeySpace) Down _ _) = flipWM . extend iterateSpace
    eventSpace (EventKey (Char 'r') Down _ _) = flipWM . extend initSpaceOperator
    eventSpace (EventKey (SpecialKey KeyEnter) Down _ _) = fmap void . flipWM . extend (spaceRandomKeyWrite)
  
