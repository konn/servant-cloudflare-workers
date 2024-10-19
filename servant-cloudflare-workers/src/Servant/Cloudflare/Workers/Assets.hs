{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Servant.Cloudflare.Workers.Assets (
  serveAssets,
  serveAssets',
  serveAssetsWith,
) where

import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding (BindingsClass)
import qualified Network.Cloudflare.Worker.Binding as Binding
import Network.Cloudflare.Worker.Binding.Assets (Assets, AssetsClass)
import qualified Network.Cloudflare.Worker.Binding.Assets as Assets
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Servant.Cloudflare.Workers
import Servant.Cloudflare.Workers.Prelude hiding (inject)

serveAssetsWith :: Assets -> (WorkerRequest -> IO WorkerRequest) -> WorkerT e Raw m
serveAssetsWith assets f = Tagged \req _ _ -> do
  await =<< Assets.fetch assets . inject =<< f req

serveAssets' ::
  forall l ->
  (Member l bs, Lookup' l bs ~ AssetsClass) =>
  (WorkerRequest -> IO WorkerRequest) ->
  WorkerT (BindingsClass env secret bs) Raw m
serveAssets' l f = Tagged \req bindings _ -> do
  let assets = Binding.getBinding l $ bindings
  await =<< Assets.fetch assets . inject =<< f req

serveAssets ::
  forall l ->
  (Member l bs, Lookup' l bs ~ AssetsClass) =>
  WorkerT (BindingsClass env secret bs) Raw m
serveAssets l = serveAssets' l pure
