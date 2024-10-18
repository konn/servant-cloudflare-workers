{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | @since 0.14.1
module Servant.Cloudflare.Workers.Generic (
  AsWorkerT,
  AsWorker,
  genericServe,
  genericServeT,
  genericServeTWithContext,
  genericServer,
  genericServerT,
) where

import Data.Kind (
  Type,
 )
import Data.Proxy (
  Proxy (..),
 )
import Servant.API.Generic
import Servant.Cloudflare.Workers
import Servant.Cloudflare.Workers.Internal

-- | Transform a record of routes into a WAI 'Application'.
genericServe ::
  forall routes.
  ( HasWorker (ToServantApi routes) '[]
  , GenericServant routes AsWorker
  , Worker (ToServantApi routes) ~ ToServant routes AsWorker
  ) =>
  routes AsWorker ->
  Application
genericServe = serve (Proxy :: Proxy (ToServantApi routes)) . genericServer

{- | Transform a record of routes with custom monad into a WAI 'Application',
  by providing a transformation to bring each handler back in the 'Handler'
  monad.
-}
genericServeT ::
  forall (routes :: Type -> Type) (m :: Type -> Type).
  ( GenericServant routes (AsWorkerT m)
  , GenericServant routes AsApi
  , HasWorker (ToServantApi routes) '[]
  , WorkerT (ToServantApi routes) m ~ ToServant routes (AsWorkerT m)
  ) =>
  -- | 'hoistWorker' argument to come back to 'Handler'
  (forall a. m a -> Handler a) ->
  -- | your record full of request handlers
  routes (AsWorkerT m) ->
  Application
genericServeT f server = serve p $ hoistWorker p f (genericServerT server)
  where
    p = genericApi (Proxy :: Proxy routes)

{- | Transform a record of routes with custom monad into a WAI 'Application',
  while using the given 'Context' to serve the application (contexts are typically
  used by auth-related combinators in servant, e.g to hold auth checks) and the given
  transformation to map all the handlers back to the 'Handler' monad.
-}
genericServeTWithContext ::
  forall (routes :: Type -> Type) (m :: Type -> Type) (ctx :: [Type]).
  ( GenericServant routes (AsWorkerT m)
  , GenericServant routes AsApi
  , HasWorker (ToServantApi routes) ctx
  , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
  , WorkerT (ToServantApi routes) m ~ ToServant routes (AsWorkerT m)
  ) =>
  -- | 'hoistWorker' argument to come back to 'Handler'
  (forall a. m a -> Handler a) ->
  -- | your record full of request handlers
  routes (AsWorkerT m) ->
  -- | the 'Context' to serve the application with
  Context ctx ->
  Application
genericServeTWithContext f server ctx =
  serveWithContext p ctx $
    hoistWorkerWithContext p pctx f (genericServerT server)
  where
    p = genericApi (Proxy :: Proxy routes)
    pctx = Proxy :: Proxy ctx

-- | Transform a record of endpoints into a 'Worker'.
genericServer ::
  (GenericServant routes AsWorker) =>
  routes AsWorker ->
  ToServant routes AsWorker
genericServer = toServant

{- | Transform a record of endpoints into a @'WorkerT' m@.

 You can see an example usage of this function
 <https://docs.servant.dev/en/stable/cookbook/generic/Generic.html#using-generics-together-with-a-custom-monad in the Servant Cookbook>.
-}
genericServerT ::
  (GenericServant routes (AsWorkerT m)) =>
  routes (AsWorkerT m) ->
  ToServant routes (AsWorkerT m)
genericServerT = toServant
