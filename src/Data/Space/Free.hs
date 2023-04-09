{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Space.Free where

import Control.Comonad.Cofree as Co
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Class
import Control.Core.Composition
import Control.Monad.Co
import Control.Monad.Free as Fr
import Control.Monad.Trans.Adjoint as M
import Data.Functor.Adjunction
import Data.Universe

type Free2D f = Free (Free f)

type Cofree2D g = Cofree (Cofree g)
