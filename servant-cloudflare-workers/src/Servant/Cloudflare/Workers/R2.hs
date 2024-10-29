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

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Wasm.Object.Builtins
import qualified GHC.Wasm.Web.Generated.Headers as Headers
import Network.Cloudflare.Worker.Binding (BindingsClass)
import qualified Network.Cloudflare.Worker.Binding as Raw
import Network.Cloudflare.Worker.Binding.R2 (R2, R2Class)
import qualified Network.Cloudflare.Worker.Binding.R2 as R2
import qualified Network.Cloudflare.Worker.Request as Req
import qualified Network.Cloudflare.Worker.Response as Resp
import Network.HTTP.Types.URI
import Network.Mime (defaultMimeLookup)
import Servant.Cloudflare.Workers
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude hiding (inject)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Wasm.Data.Function.Linear as PL

serveObjectWith :: R2 -> BS.ByteString -> WorkerT e Raw m
serveObjectWith r2 obj = Tagged \_ _ _ -> do
  mbody <- await' =<< R2.get r2 obj
  case mbody of
    Nothing ->
      toWorkerResponse $ responseServerError err500 {errBody = "Failed to fetch R2 object"}
    Just r2body -> do
      src <- R2.getBody r2body
      let etag = R2.getObjectHTTPETag r2body
          ctype = defaultMimeLookup $ TE.decodeUtf8 $ last $ BS8.split '/' obj
      hdrs <- Resp.toHeaders mempty
      R2.writeObjectHttpMetadata r2body hdrs
      let cacheHdrs =
            [ ("ETag", etag)
            , ("Content-Type", ctype)
            ]
      forM_ cacheHdrs $ \(k, v) -> do
        k' <- fromHaskellByteString k
        v' <- fromHaskellByteString v
        Headers.js_fun_set_ByteString_ByteString_undefined hdrs k' v'
      empty <- emptyObject
      Resp.newResponse' (Just $ inject src) $
        Just $
          newDictionary
            ( setPartialField "statusText" (fromBS "OK")
                PL.. setPartialField "status" (toJSPrim 200)
                PL.. setPartialField "headers" (inject hdrs)
                PL.. setPartialField "encodeBody" (fromBS "automatic")
                PL.. setPartialField "cf" empty
            )

fromBS :: BS.ByteString -> JSObject JSByteStringClass
{-# NOINLINE fromBS #-}
fromBS = unsafeDupablePerformIO . fromHaskellByteString

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
