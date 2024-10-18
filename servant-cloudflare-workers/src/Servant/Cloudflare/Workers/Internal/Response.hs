{-# LANGUAGE OverloadedRecordDot #-}

module Servant.Cloudflare.Workers.Internal.Response (
  PartialResponse (..),
  toWorkerResponse,
  responseLBS,
) where

import Control.Monad (guard)
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import Data.Word
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.JSON (encodeJSON)
import Network.Cloudflare.Worker.Response (WorkerResponse, WorkerResponseBody (..), fromWorkerResponseBody, newResponse')
import qualified Network.Cloudflare.Worker.Response as Resp
import Network.HTTP.Types
import qualified Wasm.Prelude.Linear as PL

data PartialResponse = PartialResponse
  { body :: !(Maybe WorkerResponseBody)
  , status :: !Status
  , headers :: ![Header]
  , cloudflare :: !(Maybe Value)
  , encodeBody :: !(Maybe BS.ByteString)
  }
  deriving (Generic)

toWorkerResponse :: PartialResponse -> IO WorkerResponse
toWorkerResponse PartialResponse {..} = do
  mbody <- mapM fromWorkerResponseBody body
  hdrs <- Resp.toHeaders $ Map.mapKeys CI.original $ Map.fromList headers
  encode <- maybe (fromHaskellByteString "automatic") fromHaskellByteString encodeBody
  cf <- maybe emptyObject encodeJSON cloudflare
  statusMsg <- fromHaskellByteString status.statusMessage
  newResponse' mbody $
    Just $
      newDictionary
        PL.$ PL.id
        PL.. setPartialField "status" (toJSPrim $ fromIntegral @_ @Word16 status.statusCode)
        PL.. setPartialField "statusText" statusMsg
        PL.. setPartialField "headers" (inject hdrs)
        PL.. setPartialField "encodeBody" encode
        PL.. setPartialField "cf" cf

responseLBS ::
  Status ->
  [(HeaderName, BS.StrictByteString)] ->
  LBS.ByteString ->
  PartialResponse
responseLBS status headers bdy =
  PartialResponse
    { status = status
    , headers
    , encodeBody = Nothing
    , cloudflare = Nothing
    , body = do
        guard $ not $ LBS.null bdy
        Just $ WorkerResponseLBS bdy
    }
