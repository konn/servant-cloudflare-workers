{-# LANGUAGE UndecidableInstances #-}

module Servant.Auth.Cloudflare.Workers.Internal.Class where

import Data.Kind (Type)
import GHC.Wasm.Object.Builtins (Prototype)
import Servant.Auth
import Servant.Auth.Cloudflare.Workers.Internal.BasicAuth
import Servant.Auth.Cloudflare.Workers.Internal.ConfigTypes
import Servant.Auth.Cloudflare.Workers.Internal.JWT (cloudflareZeroTrustAuthCheck, jwtAuthCheck)
import Servant.Auth.Cloudflare.Workers.Internal.Types
import Servant.Auth.JWT
import Servant.Cloudflare.Workers.Prelude hiding (BasicAuth)

{- | @IsAuth a ctx v@ indicates that @a@ is an auth type that expects all
elements of @ctx@ to be the in the Context and whose authentication check
returns an @AuthCheck v@.
-}
class IsAuth (e :: Prototype) a v where
  type AuthArgs e a :: [Type]
  runAuth :: proxy' e -> proxy a -> proxy v -> Unapp (AuthArgs e a) (AuthCheck e v)

instance (FromJWT usr) => IsAuth e JWT usr where
  type AuthArgs e JWT = '[JWTSettings e]
  runAuth _ _ _ = jwtAuthCheck

instance (FromBasicAuthData usr) => IsAuth e BasicAuth usr where
  type AuthArgs e BasicAuth = '[BasicAuthCfg]
  runAuth _ _ _ = basicAuthCheck

instance (FromJWT usr) => IsAuth e CloudflareZeroTrust usr where
  type AuthArgs e CloudflareZeroTrust = '[CloudflareZeroTrustSettings e]
  runAuth _ _ _ = cloudflareZeroTrustAuthCheck

-- * Helper

class AreAuths (e :: Prototype) (as :: [Type]) (ctxs :: [Type]) v where
  runAuths :: proxy as -> Context ctxs -> AuthCheck e v

instance AreAuths e '[] ctxs v where
  runAuths _ _ = mempty

instance
  ( AuthCheck e v ~ App (AuthArgs e a) (Unapp (AuthArgs e a) (AuthCheck e v))
  , IsAuth e a v
  , AreAuths e as ctxs v
  , AppCtx ctxs (AuthArgs e a) (Unapp (AuthArgs e a) (AuthCheck e v))
  ) =>
  AreAuths e (a ': as) ctxs v
  where
  runAuths _ ctxs = go <> runAuths (Proxy :: Proxy as) ctxs
    where
      go =
        appCtx
          (Proxy :: Proxy (AuthArgs e a))
          ctxs
          (runAuth (Proxy :: Proxy e) (Proxy :: Proxy a) (Proxy :: Proxy v))

type family Unapp ls res where
  Unapp '[] res = res
  Unapp (arg1 ': rest) res = arg1 -> Unapp rest res

type family App ls res where
  App '[] res = res
  App (arg1 ': rest) (arg1 -> res) = App rest res

{- | @AppCtx@ applies the function @res@ to the arguments in @ls@ by taking the
values from the Context provided.
-}
class AppCtx ctx ls res where
  appCtx :: proxy ls -> Context ctx -> res -> App ls res

instance
  ( HasContextEntry ctxs ctx
  , AppCtx ctxs rest res
  ) =>
  AppCtx ctxs (ctx ': rest) (ctx -> res)
  where
  appCtx _ ctx fn = appCtx (Proxy :: Proxy rest) ctx $ fn $ getContextEntry ctx

instance AppCtx ctx '[] res where
  appCtx _ _ r = r
