{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Servant.Auth.Cloudflare.Workers.Internal.ConfigTypes (
  module Servant.Auth.Cloudflare.Workers.Internal.ConfigTypes,
  Servant.API.IsSecure (..),
  JWSAlg (..),
  JsonWebKey,
  ClaimsSet (..),
  JSObject (..),
) where

import qualified Data.Aeson as J
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import Data.Default.Class
import Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.CryptoKey (CryptoKey)
import GHC.Wasm.Web.Generated.JsonWebKey.Core (JsonWebKey)
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Servant.API (IsSecure (..))
import Servant.Auth.JWT

data IsMatch = Matches | DoesNotMatch
  deriving (Eq, Show, Read, Generic, Ord)

data IsPasswordCorrect = PasswordCorrect | PasswordIncorrect
  deriving (Eq, Show, Read, Generic, Ord)

-- The @SameSite@ attribute of cookies determines whether cookies will be sent
-- on cross-origin requests.
--
-- See <https://tools.ietf.org/html/draft-west-first-party-cookies-07 this document>
-- for more information.
data SameSite = AnySite | SameSiteStrict | SameSiteLax
  deriving (Eq, Show, Read, Generic, Ord)

-- | @JWTSettings@ are used to generate cookies, and to verify JWTs.
data JWTSettings e = JWTSettings
  { validationKeys :: JSObject e -> FetchContext -> IO [(Maybe T.Text, CryptoKey)]
  -- ^ Keys used to validate JWT.
  , audienceMatches :: T.Text -> IsMatch
  -- ^ An @aud@ predicate. The @aud@ is a string or URI that identifies the
  -- intended recipient of the JWT.
  }
  deriving (Generic)

-- | A @JWTSettings@ where the audience always matches.
defaultJWTSettings :: CryptoKey -> JWTSettings e
defaultJWTSettings k =
  JWTSettings
    { validationKeys = const $ const $ pure [(Nothing, k)]
    , audienceMatches = const Matches
    }

{- | The policies to use when generating cookies.

If *both* 'cookieMaxAge' and 'cookieExpires' are @Nothing@, browsers will
treat the cookie as a *session cookie*. These will be deleted when the
browser is closed.

Note that having the setting @Secure@ may cause testing failures if you are
not testing over HTTPS.
-}
data CookieSettings = CookieSettings
  { cookieIsSecure :: !IsSecure
  -- ^ 'Secure' means browsers will only send cookies over HTTPS. Default:
  -- @Secure@.
  , cookieMaxAge :: !(Maybe DiffTime)
  -- ^ How long from now until the cookie expires. Default: @Nothing@.
  , cookieExpires :: !(Maybe UTCTime)
  -- ^ At what time the cookie expires. Default: @Nothing@.
  , cookiePath :: !(Maybe BS.ByteString)
  -- ^ The URL path and sub-paths for which this cookie is used. Default: @Just "/"@.
  , cookieDomain :: !(Maybe BS.ByteString)
  -- ^ Domain name, if set cookie also allows subdomains. Default: @Nothing@.
  , cookieSameSite :: !SameSite
  -- ^ 'SameSite' settings. Default: @SameSiteLax@.
  , sessionCookieName :: !BS.ByteString
  -- ^ What name to use for the cookie used for the session.
  , cookieXsrfSetting :: !(Maybe XsrfCookieSettings)
  -- ^ The optional settings to use for XSRF protection. Default: @Just def@.
  }
  deriving (Eq, Show, Generic)

instance Default CookieSettings where
  def = defaultCookieSettings

defaultCookieSettings :: CookieSettings
defaultCookieSettings =
  CookieSettings
    { cookieIsSecure = Secure
    , cookieMaxAge = Nothing
    , cookieExpires = Nothing
    , cookiePath = Just "/"
    , cookieDomain = Nothing
    , cookieSameSite = SameSiteLax
    , sessionCookieName = "JWT-Cookie"
    , cookieXsrfSetting = Just def
    }

-- | The policies to use when generating and verifying XSRF cookies
data XsrfCookieSettings = XsrfCookieSettings
  { xsrfCookieName :: !BS.ByteString
  -- ^ What name to use for the cookie used for XSRF protection.
  , xsrfCookiePath :: !(Maybe BS.ByteString)
  -- ^ What path to use for the cookie used for XSRF protection. Default @Just "/"@.
  , xsrfHeaderName :: !BS.ByteString
  -- ^ What name to use for the header used for XSRF protection.
  , xsrfExcludeGet :: !Bool
  -- ^ Exclude GET request method from XSRF protection.
  }
  deriving (Eq, Show, Generic)

instance Default XsrfCookieSettings where
  def = defaultXsrfCookieSettings

defaultXsrfCookieSettings :: XsrfCookieSettings
defaultXsrfCookieSettings =
  XsrfCookieSettings
    { xsrfCookieName = "XSRF-TOKEN"
    , xsrfCookiePath = Just "/"
    , xsrfHeaderName = "X-XSRF-TOKEN"
    , xsrfExcludeGet = False
    }

data CloudflareZeroTrustSettings e = CloudflareZeroTrustSettings
  { cfAudienceId :: T.Text
  , cfValidationKeys :: JSObject e -> FetchContext -> IO (HashMap T.Text CryptoKey)
  }
  deriving (Generic)

toJWTSettings :: CloudflareZeroTrustSettings e -> JWTSettings e
toJWTSettings CloudflareZeroTrustSettings {..} =
  JWTSettings
    { validationKeys = \env fctx -> map (Bi.first Just) . HM.toList <$> cfValidationKeys env fctx
    , audienceMatches = \aud -> if aud == cfAudienceId then Matches else DoesNotMatch
    }

type AudienceId = T.Text

type TeamName = String

eitherResult :: J.Result a -> Either String a
eitherResult (J.Success resl) = Right resl
eitherResult (J.Error err) = Left err
