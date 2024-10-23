{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Auth.Cloudflare.Workers.Internal where

import Control.Monad.Trans (liftIO)
import Servant.Auth
import Servant.Auth.Cloudflare.Workers.Internal.Class
import Servant.Auth.Cloudflare.Workers.Internal.ConfigTypes
import Servant.Auth.Cloudflare.Workers.Internal.JWT
import Servant.Auth.Cloudflare.Workers.Internal.Types
import Servant.Cloudflare.Workers.Internal (DelayedIO, addAuthCheck, withRequest)
import Servant.Cloudflare.Workers.Prelude (
  Handler,
  HasContextEntry (..),
  HasWorker (..),
  Proxy (..),
  (:>),
 )

instance
  ( AreAuths e auths ctxs v
  , HasWorker e api ctxs
  , ToJWT v
  , HasContextEntry ctxs (JWTSettings e)
  , HasContextEntry ctxs (CloudflareZeroTrustSettings e)
  ) =>
  HasWorker e (Auth auths v :> api) ctxs
  where
  type WorkerT e (Auth auths v :> api) m = AuthResult v -> WorkerT e api m

  hoistWorkerWithContext e _ pc nt s = hoistWorkerWithContext e (Proxy :: Proxy api) pc nt . s

  route e _ context subserver =
    route
      e
      (Proxy :: Proxy api)
      context
      (fmap go subserver `addAuthCheck` authCheck)
    where
      authCheck :: DelayedIO e (AuthResult v)
      authCheck = withRequest $ \req env c -> liftIO $ do
        authResult <- runAuthCheck (runAuths (Proxy :: Proxy auths) context) req env c
        pure (authResult)

      go ::
        (AuthResult v -> WorkerT e api (Handler e)) ->
        AuthResult v ->
        WorkerT e api (Handler e)
      go fn authResult = fn authResult
