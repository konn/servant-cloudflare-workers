module Effectful.Servant.Cloudflare.Workers.Cache (
  CacheOptions (..),
  serveCached,
  serveCachedRaw,
  serveCachedRawM,

  -- * Low-level combinators
  retrieveCache,
  saveCache,
) where

import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Servant.Cloudflare.Workers
import Servant.Cloudflare.Workers.Cache (CacheOptions (..), retrieveCache, saveCache, serveCachedRaw, serveCachedRawM)

serveCached ::
  forall es.
  (HasUniqueWorker es) =>
  CacheOptions ->
  Eff es ()
serveCached copts = do
  rawReq <- getRawRequest
  fctx <- getFetchContext
  unsafeEff_ (retrieveCache copts rawReq) >>= \case
    Right resp -> earlyReturn $ RawResponse resp
    Left keyReq ->
      addFinaliser $ saveCache copts fctx keyReq
