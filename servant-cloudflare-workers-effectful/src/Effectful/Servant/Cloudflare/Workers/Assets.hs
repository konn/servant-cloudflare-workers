{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Servant.Cloudflare.Workers.Assets (
  module Servant.Cloudflare.Workers.Assets,
  Assets,
  AssetsClass,
  fetch,
) where

import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Servant.Cloudflare.Workers
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.URL
import Network.Cloudflare.Worker.Binding.Assets hiding (fetch)
import Network.Cloudflare.Worker.Binding.Assets qualified as Raw
import Network.Cloudflare.Worker.Request
import Network.Cloudflare.Worker.Response (WorkerResponseClass)
import Servant.Cloudflare.Workers.Assets

fetch ::
  (HasUniqueWorker es) =>
  Assets ->
  Union '[WorkerRequestClass, USVStringClass, URLClass] ->
  Eff es (Promise WorkerResponseClass)
fetch = fmap unsafeEff_ . Raw.fetch
