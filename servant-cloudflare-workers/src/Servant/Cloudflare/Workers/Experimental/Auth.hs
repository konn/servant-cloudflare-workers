{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Cloudflare.Workers.Experimental.Auth where

import Control.Monad.Trans (
  liftIO,
 )
import Data.Kind (
  Type,
 )
import Data.Proxy (
  Proxy (Proxy),
 )
import Data.Typeable (
  Typeable,
 )
import GHC.Generics (
  Generic,
 )
import Network.Cloudflare.Worker.Request (
  WorkerRequest,
 )
import Servant.API.Experimental.Auth
import Servant.Cloudflare.Workers.Internal (
  Handler,
  HasContextEntry,
  HasWorker (..),
  addAuthCheck,
  delayedFailFatal,
  getContextEntry,
  runHandler,
  withRequest,
 )
import Servant.Cloudflare.Workers.Internal.DelayedIO (liftRouteResult)
import Servant.Cloudflare.Workers.Internal.Handler (Finaliser, ServerReturn (..))
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.RouteResult (RouteResult (..))
import Servant.Cloudflare.Workers.Prelude (
  (:>),
 )

-- * General Auth

{- | Specify the type of data returned after we've authenticated a request.
quite often this is some `User` datatype.

NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
-}
type family AuthServerData a :: Type

{- | Handlers for AuthProtected resources

NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
-}
newtype AuthHandler e r usr = AuthHandler
  {unAuthHandler :: r -> Handler e usr}
  deriving (Functor, Generic, Typeable)

-- | NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
mkAuthHandler :: (r -> Handler e usr) -> AuthHandler e r usr
mkAuthHandler = AuthHandler

-- | Known orphan instance.
instance
  ( HasWorker e api context
  , HasContextEntry context (AuthHandler e WorkerRequest (AuthServerData (AuthProtect tag)))
  ) =>
  HasWorker e (AuthProtect tag :> api) context
  where
  type
    WorkerT e (AuthProtect tag :> api) m =
      (AuthServerData (AuthProtect tag), Finaliser) -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context (subserver `addAuthCheck` withRequest authCheck)
    where
      authHandler = unAuthHandler (getContextEntry context)
      authCheck req wenv fctx =
        liftIO (runHandler req wenv fctx $ authHandler req)
          >>= either
            ( \case
                Error err -> delayedFailFatal err
                Response p -> liftRouteResult . FastReturn =<< liftIO (toWorkerResponse p)
            )
            return
