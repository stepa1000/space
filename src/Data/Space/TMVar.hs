{-# LANGUAGE TypeOperators #-}

module Data.Space.TMVar where

import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Core.Composition
import Control.Monad.Co
import Control.Monad.Reader
import Data.Functor.Adjunction
import Data.Space.Base
import Data.Universe

-- type SpaceTMVar a = SpaceGlobal
--

-- makeSpaceDSTMVar :: a ->
