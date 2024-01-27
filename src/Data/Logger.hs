{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The logger interface module. It should not define a specific
-- implementation.
module Data.Logger where

import Data.Yaml
import GHC.Generics
import Control.Concurrent.STM.TVar
import Control.Core.Composition
import Control.Core.Biparam
import Control.Monad.Reader
import Data.Functor.Adjunction
import Control.Comonad.Trans.Env
import Control.Comonad.Trans.Adjoint as W
import Control.Comonad
import Debug.Trace
import Data.Functor.Identity

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

logDebug,logInfo,logWarning,logError :: LogLevel -> String -> Maybe String
logDebug ll s | ll <= Debug     = Just $ show Debug ++ ":" ++ s 
logDebug _ _= Nothing
logInfo ll s | ll <= Info       = Just $ show Info ++ ":" ++ s
logInfo _ _ = Nothing
logWarning ll s | ll <= Warning = Just $ show Warning ++ ":" ++ s 
logWarning _ _ = Nothing
logError ll s | ll <= Error     = Just $ show Error ++ ":" ++ s  
logError _ _ = Nothing

-- | Concatenates a text and an instance of 'Show'. This is a
-- convenience function to make logger function applications more
-- concise:
--
-- > Log.logError (hLogger h) "The error code is " .< e
(.<) :: (Show a) => String -> a -> String
text .< a = text <> (show a)

infixr 7 .<

-- Adjoint
{-
createDebug,createInfo,createWarning,createError :: W.AdjointT (Env LogLevel) (Reader LogLevel) w ()
createDebug = createCoadj $ Identity Debug 
createInfo = createCoadj $ Identity Info 
createWarning = createCoadj $ Identity Warning 
createError = createCoadj $ Identity Error
-}
coadjLogDebugM,coadjLogInfoM,coadjLogWarningM,coadjLogErrorM :: (Comonad w,Applicative f) => 
  String -> W.AdjointT (Env LogLevel) (Reader LogLevel) w b -> f ()
coadjLogDebugM i = coadjBiparam (\ops _-> maybe (pure ()) traceM (logDebug ops i))
coadjLogInfoM i = coadjBiparam (\ops _-> maybe (pure ()) traceM (logInfo ops i))
coadjLogWarningM i = coadjBiparam (\ops _-> maybe (pure ()) traceM (logWarning ops i))
coadjLogErrorM i = coadjBiparam (\ops _-> maybe (pure ()) traceM (logError ops i))
