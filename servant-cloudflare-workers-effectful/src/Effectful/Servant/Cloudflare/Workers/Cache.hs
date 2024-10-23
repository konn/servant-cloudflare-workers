{-# LANGUAGE AllowAmbiguousTypes #-}

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
  forall e es.
  (ServantWorker e ∈ es) =>
  CacheOptions ->
  Eff es ()
serveCached copts = do
  rawReq <- getRawRequest @e
  fctx <- getFetchContext @e
  unsafeEff_ (retrieveCache copts rawReq) >>= \case
    Right resp -> earlyReturn @e $ RawResponse resp
    Left keyReq ->
      addFinaliser @e $ saveCache copts fctx keyReq
