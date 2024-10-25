module Servant.Cloudflare.Workers.ReadableStream (
  FromReadableStream (..),
  ToReadableStream (..),
) where

import GHC.Wasm.Web.Generated.ReadableStream

class FromReadableStream a where
  fromReadableStreamIO :: ReadableStream -> IO a

instance FromReadableStream ReadableStream where
  fromReadableStreamIO = pure

class ToReadableStream a where
  toReadableStream' :: a -> ReadableStream

instance ToReadableStream ReadableStream where
  toReadableStream' = id
