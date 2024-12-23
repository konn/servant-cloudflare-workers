{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Servant.Cloudflare.Workers.Cache (
  CacheOptions (..),
  serveCached,
  serveCachedRaw,
  serveCachedRawM,

  -- * Raw-level combinators
  serveCachedIO,
  retrieveCache,
  saveCache,
  CacheKeyRequest (..),
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..))
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromJust)
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import qualified GHC.Wasm.Web.Generated.Headers as Headers
import qualified Network.Cloudflare.Worker.Binding.Cache as Cache
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext, waitUntil)
import Network.Cloudflare.Worker.Request (WorkerRequest)
import qualified Network.Cloudflare.Worker.Request as Req
import Network.Cloudflare.Worker.Response (WorkerResponse)
import qualified Network.Cloudflare.Worker.Response as Resp
import Network.URI (parseURI)
import Servant.API (URI (..))
import Servant.API.Raw
import Servant.Cloudflare.Workers (Handler, WorkerT, earlyReturn)
import Servant.Cloudflare.Workers.Internal.Handler (HandlerEnv (..), addFinaliser)
import Servant.Cloudflare.Workers.Internal.Response (RoutingResponse (RawResponse))
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import qualified Wasm.Prelude.Linear as PL

data CacheOptions = CacheOptions
  { cacheTTL :: !Word32
  , onlyOk :: !Bool
  , includeQuery :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)

serveCached :: CacheOptions -> Handler e ()
serveCached copts = do
  HandlerEnv {..} <- ask
  liftIO (retrieveCache copts request.rawRequest) >>= \case
    Right resp -> earlyReturn $ RawResponse resp
    Left keyReq ->
      addFinaliser $ saveCache copts fetchContext keyReq

serveCachedRaw :: CacheOptions -> WorkerT e Raw m -> WorkerT e Raw m
serveCachedRaw opts (Tagged app) = Tagged \req env fctx ->
  serveCachedIO opts req.rawRequest fctx (app req env fctx)

serveCachedRawM :: (MonadIO m) => CacheOptions -> WorkerT e RawM m -> WorkerT e RawM m
serveCachedRawM opts act req env ctx respond =
  serveCachedIO opts req.rawRequest ctx (act req env ctx respond)

serveCachedIO ::
  (MonadIO m) =>
  CacheOptions ->
  WorkerRequest ->
  FetchContext ->
  m WorkerResponse ->
  m WorkerResponse
serveCachedIO opts req ctx act = do
  mcache <- liftIO $ retrieveCache opts req
  case mcache of
    Right resp -> pure resp
    Left keyReq -> do
      resp <- act
      liftIO $ saveCache opts ctx keyReq resp
      pure resp

saveCache :: CacheOptions -> FetchContext -> CacheKeyRequest -> WorkerResponse -> IO ()
saveCache opts ctx keyReq resp = do
  code <- Resp.getStatus resp
  when (not opts.onlyOk || code == 200) do
    respHdrs0 <- Resp.getHeaders resp
    cacheControlHdr <- fromHaskellByteString "Cache-Control"
    cacheControl <-
      fromHaskellByteString $
        "public, max-age=" <> BS8.pack (show opts.cacheTTL)
    Headers.js_fun_set_ByteString_ByteString_undefined
      respHdrs0
      cacheControlHdr
      cacheControl
    waitUntil ctx =<< Cache.put keyReq.cacheKeyRequest resp

newtype CacheKeyRequest = CacheKeyRequest {cacheKeyRequest :: WorkerRequest}

retrieveCache :: CacheOptions -> WorkerRequest -> IO (Either CacheKeyRequest WorkerResponse)
retrieveCache opts req = do
  let uri = fromJust $ parseURI $ T.unpack $ Req.getUrl req
      cachePath =
        T.pack $
          show $
            uri
              { uriFragment = ""
              , uriQuery = if opts.includeQuery then uri.uriQuery else ""
              }
  reqHdrs0 <- Resp.toHeaders $ map (Bi.first CI.mk) $ Req.getHeaders req
  keyReq <-
    Req.newRequest (Just cachePath) $
      Just $
        newDictionary
          PL.$ setPartialField "headers" (upcast reqHdrs0)
  fmap (nullable (Left (CacheKeyRequest keyReq)) Right) . await
    =<< Cache.match (inject keyReq) Nothing
