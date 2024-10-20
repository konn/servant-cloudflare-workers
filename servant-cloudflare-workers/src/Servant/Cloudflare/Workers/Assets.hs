{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Servant.Cloudflare.Workers.Assets (
  serveAssets,
  serveAssetsRel,
  serveAssets',
  serveAssetsWith,
) where

import qualified Data.Text as T
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding (BindingsClass)
import qualified Network.Cloudflare.Worker.Binding as Binding
import Network.Cloudflare.Worker.Binding.Assets (Assets, AssetsClass)
import qualified Network.Cloudflare.Worker.Binding.Assets as Assets
import Network.Cloudflare.Worker.Request (WorkerRequest)
import qualified Network.Cloudflare.Worker.Request as Req
import Servant.Cloudflare.Workers
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest (..))
import Servant.Cloudflare.Workers.Prelude hiding (inject)
import qualified Wasm.Prelude.Linear as PL

serveAssetsWith :: Assets -> (RoutingRequest -> IO WorkerRequest) -> WorkerT e Raw m
serveAssetsWith assets f = Tagged \req _ _ -> do
  await =<< Assets.fetch assets . inject =<< f req

serveAssets' ::
  forall l ->
  (Member l bs, Lookup' l bs ~ AssetsClass) =>
  (RoutingRequest -> IO WorkerRequest) ->
  WorkerT (BindingsClass env secret bs) Raw m
serveAssets' l f = Tagged \req bindings _ -> do
  let assets = Binding.getBinding l $ bindings
  await =<< Assets.fetch assets . inject =<< f req

{- | Relaying the request to the assets with the absolute path inherited from the raw request.

See also: 'serveAssetsRel'.
-}
serveAssets ::
  forall l ->
  (Member l bs, Lookup' l bs ~ AssetsClass) =>
  WorkerT (BindingsClass env secret bs) Raw m
serveAssets l = serveAssets' l (pure . rawRequest)

{- | Relaying the request to the assets with the path /relative/ to the current endpoint.

See also: 'serveAssets'.
-}
serveAssetsRel ::
  forall l ->
  (Member l bs, Lookup' l bs ~ AssetsClass) =>
  WorkerT (BindingsClass env secret bs) Raw m
serveAssetsRel l = serveAssets' l \RoutingRequest {..} -> do
  Req.newRequest
    (Just $ T.intercalate "/" pathInfo)
    $ Just
    $ newDictionary
      PL.$ setPartialField "cf" none
      PL.. setPartialField "method" (nonNull $ Req.getMethod rawRequest)
      PL.. setPartialField "headers" (nonNull $ Req.getHeaders' rawRequest)
      PL.. setPartialField "body" (nullable none (nonNull . inject) $ Req.getBody rawRequest)
      PL.. setPartialField "redirect" (nonNull $ unsafeCast $ Req.getRedirect rawRequest)
