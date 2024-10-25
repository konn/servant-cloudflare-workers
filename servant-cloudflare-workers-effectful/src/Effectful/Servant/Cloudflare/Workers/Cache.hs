{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Servant.Cloudflare.Workers.Cache (
  CacheOptions (..),
  serveCached,
  serveCachedRaw,
  serveCachedRawM,

  -- * Low-level combinators
  retrieveCache,
  saveCache,
) where

import Effectful.Dispatch.Static (unEff, unsafeEff, unsafeEff_)
import Effectful.Servant.Cloudflare.Workers
import Servant.API (RawM)
import Servant.Cloudflare.Workers (WorkerT)
import Servant.Cloudflare.Workers.Cache (CacheOptions (..), retrieveCache, saveCache, serveCachedIO, serveCachedRaw)

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

serveCachedRawM :: (HasUniqueWorker es) => CacheOptions -> WorkerT e RawM (Eff es) -> WorkerT e RawM (Eff es)
serveCachedRawM opts act req env ctx respond =
  unsafeEff $ \es -> serveCachedIO opts req.rawRequest ctx (unEff (act req env ctx respond) es)
