{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Servant.Cloudflare.Workers.UVerb (
  respond,
  IsServerResource,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.SOP (I (I))
import Data.SOP.Constraint (All, And)
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.HTTP.Types (HeaderName, Status, hContentType)
import Servant.API (ReflectMethod, reflectMethod)
import Servant.API.ContentTypes (AllCTRender (handleAcceptH), AllMime)
import Servant.API.ResponseHeaders (GetHeaders (..), Headers (..))
import Servant.API.UVerb (HasStatus, IsMember, Statuses, UVerb, Union, Unique, WithStatus (..), foldMapUnion, inject, statusOf)
import Servant.Cloudflare.Workers.Internal (Context, Delayed, Handler, HasWorker (..), RouteResult (FailFatal, Route), Router, Worker, WorkerT, acceptCheck, addAcceptCheck, addMethodCheck, allowedMethodHead, err406, getAcceptHeader, leafRouter, methodCheck, runAction)
import Servant.Cloudflare.Workers.Internal.Response
import Servant.Cloudflare.Workers.Internal.RoutingApplication

{- | 'return' for 'UVerb' handlers.  Takes a value of any of the members of the open union,
and will construct a union value in an 'Applicative' (eg. 'Worker').
-}
respond ::
  forall (x :: Type) (xs :: [Type]) (f :: Type -> Type).
  (Applicative f, HasStatus x, IsMember x xs) =>
  x ->
  f (Union xs)
respond = pure . inject . I

class IsServerResource (cts :: [Type]) a where
  resourceResponse :: WorkerRequest -> Proxy cts -> a -> Maybe (BSL.ByteString, BSL.ByteString)
  resourceHeaders :: Proxy cts -> a -> [(HeaderName, B.ByteString)]

instance
  {-# OVERLAPPABLE #-}
  (AllCTRender cts a) =>
  IsServerResource cts a
  where
  resourceResponse request p res = handleAcceptH p (getAcceptHeader request) res
  resourceHeaders _ _ = []

instance
  {-# OVERLAPPING #-}
  (IsServerResource cts a, GetHeaders (Headers h a)) =>
  IsServerResource cts (Headers h a)
  where
  resourceResponse request p res = resourceResponse request p (getResponse res)
  resourceHeaders cts res = getHeaders res ++ resourceHeaders cts (getResponse res)

instance
  {-# OVERLAPPING #-}
  (IsServerResource cts a) =>
  IsServerResource cts (WithStatus n a)
  where
  resourceResponse request p (WithStatus x) = resourceResponse request p x
  resourceHeaders cts (WithStatus x) = resourceHeaders cts x

encodeResource ::
  forall a cts.
  (IsServerResource cts a, HasStatus a) =>
  WorkerRequest ->
  Proxy cts ->
  a ->
  (Status, Maybe (BSL.ByteString, BSL.ByteString), [(HeaderName, B.ByteString)])
encodeResource request cts res =
  ( statusOf (Proxy @a)
  , resourceResponse request cts res
  , resourceHeaders cts res
  )

type IsServerResourceWithStatus cts = IsServerResource cts `And` HasStatus

instance
  ( ReflectMethod method
  , AllMime contentTypes
  , All (IsServerResourceWithStatus contentTypes) as
  , Unique (Statuses as) -- for consistency with servant-swagger (server would work fine
  -- without; client is a bit of a corner case, because it dispatches
  -- the parser based on the status code.  with this uniqueness
  -- constraint it won't have to run more than one parser in weird
  -- corner cases.
  ) =>
  HasWorker e (UVerb method contentTypes as) context
  where
  type WorkerT e (UVerb method contentTypes as) m = m (Union as)

  hoistWorkerWithContext _ _ _ nt s = nt s

  route ::
    forall env.
    Proxy e ->
    Proxy (UVerb method contentTypes as) ->
    Context context ->
    Delayed e env (Worker e (UVerb method contentTypes as)) ->
    Router e env
  route _ _proxy _ctx action = leafRouter route'
    where
      method = reflectMethod (Proxy @method)
      route' env request b ctx cont = do
        let action' :: Delayed e env (Handler e (Union as))
            action' =
              action
                `addMethodCheck` methodCheck method request
                `addAcceptCheck` acceptCheck (Proxy @contentTypes) (getAcceptHeader request.rawRequest)

        runAction action' env request b ctx cont $ \(output :: Union as) -> do
          let cts = Proxy @contentTypes
              pickResource :: Union as -> (Status, Maybe (BSL.ByteString, BSL.ByteString), [(HeaderName, B.ByteString)])
              pickResource = foldMapUnion (Proxy @(IsServerResourceWithStatus contentTypes)) (encodeResource request.rawRequest cts)
          case pickResource output of
            (_, Nothing, _) -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            (status, Just (contentT, body), headers) ->
              let bdy = if allowedMethodHead method request then "" else body
               in Route $ responseLBS status ((hContentType, BSL.toStrict contentT) : headers) bdy
