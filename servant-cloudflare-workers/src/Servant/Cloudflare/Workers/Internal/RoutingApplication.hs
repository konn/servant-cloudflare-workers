{-# LANGUAGE OverloadedRecordDot #-}

module Servant.Cloudflare.Workers.Internal.RoutingApplication where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext, FetchHandler)
import Network.Cloudflare.Worker.Request (WorkerRequest)
import qualified Network.Cloudflare.Worker.Request as Req
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Network.HTTP.Types.URI (decodePath, extractPath)
import Servant.Cloudflare.Workers.Internal.Response
import Servant.Cloudflare.Workers.Internal.RouteResult
import Servant.Cloudflare.Workers.Internal.ServerError

data RoutingRequest
  = RoutingRequest {rawRequest :: WorkerRequest, pathInfo :: [T.Text]}

type RoutingApplication e =
  -- | the request, the field 'pathInfo' may be modified by url routing
  RoutingRequest ->
  JSObject e ->
  FetchContext ->
  (RouteResult RoutingResponse -> IO WorkerResponse) ->
  IO WorkerResponse

toFetchHandler :: RoutingApplication e -> FetchHandler e
toFetchHandler ra rawRequest env ctx =
  let pathInfo =
        fst $
          decodePath $
            extractPath $
              TE.encodeUtf8 $
                Req.getUrl rawRequest
      req0 = RoutingRequest {..}
   in ra req0 env ctx routingRespond
  where
    routingRespond :: RouteResult RoutingResponse -> IO WorkerResponse
    routingRespond (Fail err) = toWorkerResponse (responseServerError err)
    routingRespond (FailFatal err) = toWorkerResponse $ responseServerError err
    routingRespond (Route rsp) = toWorkerResponse rsp
    routingRespond (FastReturn w) = pure w
