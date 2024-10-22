{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Auth.Client.Internal where

import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.Sequence ((<|))
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import Servant.API ((:>))
import Servant.Auth
import Servant.Client.Core

-- | A simple token.
data Token
  = BearerToken !BS.ByteString
  | CloudflareToken !(Maybe ServiceToken)
  deriving (Eq, Show, Read, Generic)

data ServiceToken = ServiceToken {clientId, clientSecret :: !BS.ByteString}
  deriving (Eq, Show, Read, Generic)

type family HasBearer xs :: Constraint where
  HasBearer (Bearer ': xs) = ()
  HasBearer (JWT ': xs) = ()
  HasBearer (x ': xs) = HasBearer xs
  HasBearer '[] = BearerAuthNotEnabled

class BearerAuthNotEnabled

{- | @'HasBearer' auths@ is nominally a redundant constraint, but ensures we're not
trying to send a token to an API that doesn't accept them.
-}
instance (HasBearer auths, HasClient m api) => HasClient m (Auth auths a :> api) where
  type Client m (Auth auths a :> api) = Token -> Client m api

  clientWithRoute m _ req (BearerToken token) =
    clientWithRoute m (Proxy :: Proxy api) $
      req {requestHeaders = ("Authorization", headerVal) <| requestHeaders req}
    where
      headerVal = "Bearer " <> token
  clientWithRoute m _ req (CloudflareToken Nothing) =
    clientWithRoute m (Proxy :: Proxy api) req
  clientWithRoute m _ req (CloudflareToken (Just ServiceToken {..})) =
    clientWithRoute m (Proxy :: Proxy api) $
      req
        { requestHeaders =
            ("CF-Access-Client-Id", clientId)
              <| ("CF-Access-Client-Secret", clientSecret)
              <| requestHeaders req
        }

  hoistClientMonad pm _ nt cl = hoistClientMonad pm (Proxy :: Proxy api) nt . cl

-- * Authentication combinators

{- | A Bearer token in the Authorization header:

   @Authorization: Bearer <token>@

This can be any token recognized by the server, for example,
a JSON Web Token (JWT).

Note that, since the exact way the token is validated is not specified,
this combinator can only be used in the client. The server would not know
how to validate it, while the client does not care.
If you want to implement Bearer authentication in your server, you have to
choose a specific combinator, such as 'JWT'.
-}
data Bearer
