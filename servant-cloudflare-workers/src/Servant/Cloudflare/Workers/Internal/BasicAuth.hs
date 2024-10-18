{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Cloudflare.Workers.Internal.BasicAuth where

import Control.Monad (
  guard,
 )
import Control.Monad.Trans (
  liftIO,
 )
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (
  decodeLenient,
 )
import qualified Data.CaseInsensitive as CI
import Data.Typeable (
  Typeable,
 )
import Data.Word8 (
  isSpace,
  toLower,
  _colon,
 )
import GHC.Generics
import Network.Cloudflare.Worker.Request (WorkerRequest)
import qualified Network.Cloudflare.Worker.Request as Req
import Network.HTTP.Types (
  Header,
 )
import Servant.API.BasicAuth (
  BasicAuthData (BasicAuthData),
 )
import Servant.Cloudflare.Workers.Internal.DelayedIO
import Servant.Cloudflare.Workers.Internal.ServerError

-- * Basic Auth

{- | servant-server's current implementation of basic authentication is not
immune to certain kinds of timing attacks. Decoding payloads does not take
a fixed amount of time.
-}

-- | The result of authentication/authorization
data BasicAuthResult usr
  = Unauthorized
  | BadPassword
  | NoSuchUser
  | Authorized usr
  deriving (Eq, Show, Read, Generic, Typeable, Functor)

-- | Datatype wrapping a function used to check authentication.
newtype BasicAuthCheck usr = BasicAuthCheck
  { unBasicAuthCheck ::
      BasicAuthData ->
      IO (BasicAuthResult usr)
  }
  deriving (Generic, Typeable, Functor)

-- | Internal method to make a basic-auth challenge
mkBAChallengerHdr :: BS.ByteString -> Header
mkBAChallengerHdr realm = ("WWW-Authenticate", "Basic realm=\"" <> realm <> "\"")

-- | Find and decode an 'Authorization' header from the request as Basic Auth
decodeBAHdr :: WorkerRequest -> Maybe BasicAuthData
decodeBAHdr req = do
  ah <- lookup "Authorization" $ map (Bi.first CI.mk) $ Req.getHeaders req
  let (b, rest) = BS.break isSpace ah
  guard (BS.map toLower b == "basic")
  let decoded = decodeLenient (BS.dropWhile isSpace rest)
  let (username, passWithColonAtHead) = BS.break (== _colon) decoded
  (_, password) <- BS.uncons passWithColonAtHead
  return (BasicAuthData username password)

{- | Run and check basic authentication, returning the appropriate http error per
the spec.
-}
runBasicAuth :: WorkerRequest -> BS.ByteString -> BasicAuthCheck usr -> DelayedIO e usr
runBasicAuth req realm (BasicAuthCheck ba) =
  case decodeBAHdr req of
    Nothing -> plzAuthenticate
    Just e ->
      liftIO (ba e) >>= \res -> case res of
        BadPassword -> plzAuthenticate
        NoSuchUser -> plzAuthenticate
        Unauthorized -> delayedFailFatal err403
        Authorized usr -> return usr
  where
    plzAuthenticate = delayedFailFatal err401 {errHeaders = [mkBAChallengerHdr realm]}
