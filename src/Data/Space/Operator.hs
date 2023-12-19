{-# LANGUAGE TypeOperators #-}

module Data.Space.Operator where

import Control.Comonad
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Core.Composition
import Control.Monad.Reader
import Data.Functor.Adjunction
import GHC.Generics
import Data.Array.MArray

type Operator a = TArray (Int,Int) a
