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
  genericWorker,
  genericWorkerT,
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
  forall e routes.
  ( HasWorker e (ToServantApi routes) '[]
  , GenericServant routes (AsWorker e)
  , Worker e (ToServantApi routes) ~ ToServant routes (AsWorker e)
  ) =>
  routes (AsWorker e) ->
  FetchHandler e
genericServe = serve (Proxy @e) (Proxy :: Proxy (ToServantApi routes)) . genericWorker

{- | Transform a record of routes with custom monad into a WAI 'Application',
  by providing a transformation to bring each handler back in the 'Handler'
  monad.
-}
genericServeT ::
  forall e (routes :: Type -> Type) (m :: Type -> Type).
  ( GenericServant routes (AsWorkerT e m)
  , GenericServant routes AsApi
  , HasWorker e (ToServantApi routes) '[]
  , WorkerT e (ToServantApi routes) m ~ ToServant routes (AsWorkerT e m)
  ) =>
  -- | 'hoistWorker' argument to come back to 'Handler'
  (forall a. m a -> Handler e a) ->
  -- | your record full of request handlers
  routes (AsWorkerT e m) ->
  FetchHandler e
genericServeT f worker = serve pe p $ hoistWorker pe p f (genericWorkerT worker)
  where
    p = genericApi (Proxy :: Proxy routes)
    pe = Proxy @e

{- | Transform a record of routes with custom monad into a WAI 'Application',
  while using the given 'Context' to serve the application (contexts are typically
  used by auth-related combinators in servant, e.g to hold auth checks) and the given
  transformation to map all the handlers back to the 'Handler' monad.
-}
genericServeTWithContext ::
  forall e (routes :: Type -> Type) (m :: Type -> Type) (ctx :: [Type]).
  ( GenericServant routes (AsWorkerT e m)
  , GenericServant routes AsApi
  , HasWorker e (ToServantApi routes) ctx
  , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
  , WorkerT e (ToServantApi routes) m ~ ToServant routes (AsWorkerT e m)
  ) =>
  -- | 'hoistWorker' argument to come back to 'Handler'
  (forall a. m a -> Handler e a) ->
  -- | your record full of request handlers
  routes (AsWorkerT e m) ->
  -- | the 'Context' to serve the application with
  Context ctx ->
  FetchHandler e
genericServeTWithContext f worker ctx =
  serveWithContext pe p ctx $
    hoistWorkerWithContext pe p pctx f (genericWorkerT @e worker)
  where
    pe = Proxy :: Proxy e
    p = genericApi (Proxy :: Proxy routes)
    pctx = Proxy :: Proxy ctx

-- | Transform a record of endpoints into a 'Worker'.
genericWorker ::
  forall e routes.
  (GenericServant routes (AsWorker e)) =>
  routes (AsWorker e) ->
  ToServant routes (AsWorker e)
genericWorker = toServant

{- | Transform a record of endpoints into a @'WorkerT' m@.

 You can see an example usage of this function
 <https://docs.servant.dev/en/stable/cookbook/generic/Generic.html#using-generics-together-with-a-custom-monad in the Servant Cookbook>.
-}
genericWorkerT ::
  forall e routes m.
  (GenericServant routes (AsWorkerT e m)) =>
  routes (AsWorkerT e m) ->
  ToServant routes (AsWorkerT e m)
genericWorkerT = toServant
