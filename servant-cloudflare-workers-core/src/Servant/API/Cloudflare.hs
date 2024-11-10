{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.API.Cloudflare (
  ReadableStreamBody,
) where

import Data.Kind
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.Wasm.Web.Generated.ReadableStream (ReadableStream)
import GHC.Wasm.Web.ReadableStream (toReadableStream)
import Network.Cloudflare.Worker.Response (WorkerResponse, WorkerResponseBody (..))
import Network.Cloudflare.Worker.Response qualified as Resp
import Servant.API
import Streaming.ByteString qualified as Q
import System.IO.Unsafe (unsafePerformIO)

{- | A request body, that is passed as a @ReadableStream@ on worker side,
and given by any type on the client side.
-}
data ReadableStreamBody (contentTypes :: [Type]) (a :: Type)
  deriving (Generic)

instance (HasLink sub) => HasLink (ReadableStreamBody contentTypes a :> sub) where
  type MkLink (ReadableStreamBody contentTypes a :> sub) x = MkLink sub x
  toLink toA _ = toLink toA (Proxy @sub)
  {-# INLINE toLink #-}

instance (Accept ct) => MimeUnrender ct ReadableStream where
  mimeUnrender _ = Right . unsafePerformIO . toReadableStream . Q.fromLazy
  {-# NOINLINE mimeUnrender #-}

instance (Accept ct) => MimeUnrender ct WorkerResponse where
  mimeUnrender _ x = Right $ unsafePerformIO do
    Resp.newResponse
      Resp.SimpleResponseInit
        { Resp.statusText = "Dummy"
        , Resp.status = 200
        , Resp.headers = mempty
        , Resp.body = Just $ WorkerResponseLBS x
        }
  {-# NOINLINE mimeUnrender #-}
