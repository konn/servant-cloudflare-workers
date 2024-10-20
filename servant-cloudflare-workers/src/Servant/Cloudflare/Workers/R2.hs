{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Servant.Cloudflare.Workers.R2 (
  -- * Serve a single object from an R2 bucket
  serveObject,
  serveObjectWith,

  -- * Serve contents from the R2 bucket
  serveBucket,
  serveBucketFrom,
  serveBucketRel,
  serveBucketRelFrom,
) where

import Control.Concurrent.Async (wait)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding (BindingsClass)
import qualified Network.Cloudflare.Worker.Binding as Raw
import Network.Cloudflare.Worker.Binding.R2 (R2, R2Class)
import qualified Network.Cloudflare.Worker.Binding.R2 as R2
import qualified Network.Cloudflare.Worker.Request as Req
import Network.Cloudflare.Worker.Response (WorkerResponseBody (WorkerResponseStream))
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI
import Servant.Cloudflare.Workers
import Servant.Cloudflare.Workers.Internal.Response (PartialResponse (..), RoutingResponse (RouteResponse), toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude hiding (inject)

serveObjectWith :: R2 -> BS.ByteString -> WorkerT e Raw m
serveObjectWith r2 obj = Tagged \_ _ _ -> do
  mbody <- wait =<< R2.get r2 obj
  case mbody of
    Nothing ->
      toWorkerResponse $ responseServerError err500 {errBody = "Failed to fetch R2 object"}
    Just r2body -> do
      src <- R2.getBody r2body
      toWorkerResponse $
        RouteResponse
          PartialResponse
            { status = status200
            , headers = []
            , encodeBody = Nothing
            , cloudflare = Nothing
            , body = Just $ WorkerResponseStream src
            }

serveObject ::
  forall l ->
  (Member l bs, Lookup l bs ~ 'Just R2Class) =>
  BS.ByteString ->
  WorkerT (BindingsClass es rs bs) Raw m
serveObject l obj = Tagged \req e fctx -> do
  let r2 = Raw.getBinding l e
  unTagged (serveObjectWith r2 obj) req e fctx

{- | Serve the content of the R2 bucket with the _absolute_ path inherited from the raw request.

See also: 'serveBucketRelFrom', 'serveBucket', and 'serveBucketRel'.
-}
serveBucketFrom ::
  R2 ->
  WorkerT (BindingsClass es rs bs) Raw m
serveBucketFrom r2 = Tagged \req bs ctx -> do
  let path =
        TE.encodeUtf8 $
          T.intercalate "/" $
            fst $
              decodePath $
                extractPath $
                  TE.encodeUtf8 $
                    Req.getUrl req.rawRequest
  unTagged (serveObjectWith r2 path) req bs ctx

{- | Serve the content of the R2 bucket with by querying the path _relative_ to the current endpoint.

See also: 'serveBucketFrom', 'serveBucket', and 'serveBucketRel'.
-}
serveBucketRelFrom ::
  R2 ->
  WorkerT (BindingsClass es rs bs) Raw m
serveBucketRelFrom r2 = Tagged \req bs ctx -> do
  let path =
        TE.encodeUtf8 $
          T.intercalate "/" req.pathInfo
  unTagged (serveObjectWith r2 path) req bs ctx

{- | Serve the content of the R2 bucket with the _absolute_ path inherited from the raw request.

See also: 'serveBucketFrom', 'serveBucketRelFrom', and 'serveBucketRel'.
-}
serveBucket ::
  forall l ->
  (Member l bs, Lookup l bs ~ 'Just R2Class) =>
  WorkerT (BindingsClass es rs bs) Raw m
serveBucket l = Tagged \req e fctx -> do
  let r2 = Raw.getBinding l e
  unTagged (serveBucketFrom r2) req e fctx

{- | Serve the content of the R2 bucket with the _absolute_ path inherited from the raw request.

See also: 'serveBucketFrom', 'serveBucketRelFrom', and 'serveBucketRel'.
-}
serveBucketRel ::
  forall l ->
  (Member l bs, Lookup l bs ~ 'Just R2Class) =>
  WorkerT (BindingsClass es rs bs) Raw m
serveBucketRel l = Tagged \req e fctx -> do
  let r2 = Raw.getBinding l e
  unTagged (serveBucketRelFrom r2) req e fctx
