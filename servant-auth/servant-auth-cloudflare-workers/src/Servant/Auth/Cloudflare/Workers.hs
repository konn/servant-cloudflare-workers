{-# LANGUAGE TypeApplications #-}

module Servant.Auth.Cloudflare.Workers (
  -- | This package provides implementations for some common authentication
  -- methods. Authentication yields a trustworthy (because generated by the
  -- server) value of an some arbitrary type:
  --
  -- > type MyApi = Protected
  -- >
  -- > type Protected = Auth '[JWT, Cookie] User :> Get '[JSON] UserAccountDetails
  -- >
  -- > server :: Server Protected
  -- > server (Authenticated usr) = ... -- here we know the client really is
  -- >                                  -- who she claims to be
  -- > server _ = throwAll err401
  --
  -- Additional configuration happens via 'Context'.
  --
  -- == Example for Custom Handler
  -- To use a custom 'Servant.Cloudflare.Workers.Handler' it is necessary to use
  -- 'Servant.Cloudflare.Workers.hoistServerWithContext' instead of
  -- 'Servant.Cloudflare.Workers.hoistServer' and specify the 'Context'.
  --
  -- Below is an example of passing 'JWTSettings' in the
  -- 'Context' to create a specialized function equivalent to
  -- 'Servant.Cloudflare.Workers.hoistServer' for an API that includes cookie
  -- authentication.
  --
  -- > hoistServerWithAuth
  -- >   :: HasServer api '[JWTSettings]
  -- >   => Proxy api
  -- >   -> (forall x. m x -> n x)
  -- >   -> ServerT api m
  -- >   -> ServerT api n
  -- > hoistServerWithAuth api =
  -- >   hoistServerWithContext api (Proxy :: Proxy '[JWTSettings])

  ----------------------------------------------------------------------------

  -- * Auth

  -- | Basic types
  Auth,
  AuthResult (..),
  AuthCheck (..),
  ----------------------------------------------------------------------------

  -- * JWT

  -- | JSON Web Tokens (JWT) are a compact and secure way of transferring
  -- information between parties. In this library, they are signed by the
  -- server (or by some other party posessing the relevant key), and used to
  -- indicate the bearer's identity or authorization.
  --
  -- Arbitrary information can be encoded - just declare instances for the
  -- 'FromJWT' and 'ToJWT' classes. Don't go overboard though - be aware that
  -- usually you'll be trasmitting this information on each request (and
  -- response!).
  --
  -- Note that, while the tokens are signed, they are not encrypted. Do not put
  -- any information you do not wish the client to know in them!

  -- ** Combinator

  -- | Re-exported from 'servant-auth'
  JWT,
  CloudflareZeroTrust,

  -- ** Classes
  FromJWT (..),
  ToJWT (..),

  -- ** Related types
  IsMatch (..),

  -- ** Settings
  JWTSettings (..),
  defaultJWTSettings,
  toJWTSettings,
  CloudflareZeroTrustSettings (..),
  defaultCloudflareZeroTrustSettings,

  -- ** Create check
  jwtAuthCheck,
  cloudflareZeroTrustAuthCheck,
  ----------------------------------------------------------------------------

  -- * Cookie

  -- | Cookies are also a method of identifying and authenticating a user. They
  -- are particular common when the client is a browser

  -- ** Combinator

  -- | Re-exported from 'servant-auth'
  Cookie,

  -- ** Settings
  CookieSettings (..),
  XsrfCookieSettings (..),
  defaultCookieSettings,
  defaultXsrfCookieSettings,

  -- ** Related types
  IsSecure (..),
  SameSite (..),
  AreAuths,
  ----------------------------------------------------------------------------

  -- * BasicAuth

  -- ** Combinator

  -- | Re-exported from 'servant-auth'
  BasicAuth,

  -- ** Classes

  -- ** Related types
  BasicAuthData (..),
  IsPasswordCorrect (..),
  ----------------------------------------------------------------------------

  -- * Utilies
  ThrowAll (throwAll),
  generateKey,
  verifyJWT,

  -- ** Re-exports
  Default (def),
  SetCookie,
) where

import Data.Default.Class (Default (def))
import GHC.IsList (IsList (..))
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AlgorithmIdentifier (AlgorithmIdentifier)
import GHC.Wasm.Web.Generated.SubtleCrypto (js_fun_generateKey_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any)
import Network.Cloudflare.Worker.Crypto (subtleCrypto)
import Servant.Auth
import Servant.Auth.Cloudflare.Workers.Internal ()
import Servant.Auth.Cloudflare.Workers.Internal.Class
import Servant.Auth.Cloudflare.Workers.Internal.ConfigTypes
import Servant.Auth.Cloudflare.Workers.Internal.JWT
import Servant.Auth.Cloudflare.Workers.Internal.ThrowAll
import Servant.Auth.Cloudflare.Workers.Internal.Types
import Servant.Cloudflare.Workers.Prelude (BasicAuthData (..))
import Web.Cookie (SetCookie)
import Prelude hiding (readFile, writeFile)

-- | Generate a key suitable for use with 'defaultConfig'.
generateKey :: IO CryptoKey
generateKey =
  privateKeyOf
    =<< await
    =<< js_fun_generateKey_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
      subtleCrypto
      rs256Params
      False
      (toSequence $ fromList ["sign", "verify"])

foreign import javascript unsafe "{name: \"RSASSA-PKCS1-v1_5\", hash: \"SHA-256\", publicExponent: Uint8Array.from([0x01, 0x00, 0x01]), modulusLength: 4096}"
  rs256Params :: AlgorithmIdentifier

foreign import javascript unsafe "$1.privateKey"
  privateKeyOf :: JSAny -> IO CryptoKey
