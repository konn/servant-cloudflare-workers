module Servant.Cloudflare.Workers.ReadableStream (
  FromReadableStream (..),
  ToReadableStream (..),
) where

import GHC.Wasm.Web.Generated.ReadableStream
import GHC.Wasm.Web.ReadableStream
import qualified Streaming.ByteString as Q

class FromReadableStream a where
  fromReadableStreamIO :: ReadableStream -> IO a

instance FromReadableStream ReadableStream where
  fromReadableStreamIO = pure

instance FromReadableStream (Q.ByteStream IO ()) where
  fromReadableStreamIO = pure . fromReadableStream

class ToReadableStream a where
  toReadableStream' :: a -> ReadableStream

instance ToReadableStream ReadableStream where
  toReadableStream' = id
