{-# LANGUAGE UndecidableInstances #-}

module Servant.Auth.Cloudflare.Workers.Internal.Class where

import Data.Kind (Type)
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
class IsAuth a v where
  type AuthArgs a :: [Type]
  runAuth :: proxy a -> proxy v -> Unapp (AuthArgs a) (AuthCheck v)

instance (FromJWT usr) => IsAuth JWT usr where
  type AuthArgs JWT = '[JWTSettings]
  runAuth _ _ = jwtAuthCheck

instance (FromBasicAuthData usr) => IsAuth BasicAuth usr where
  type AuthArgs BasicAuth = '[BasicAuthCfg]
  runAuth _ _ = basicAuthCheck

instance (FromJWT usr) => IsAuth CloudflareZeroTrust usr where
  type AuthArgs CloudflareZeroTrust = '[CloudflareZeroTrustSettings]
  runAuth _ _ = cloudflareZeroTrustAuthCheck

-- * Helper

class AreAuths (as :: [Type]) (ctxs :: [Type]) v where
  runAuths :: proxy as -> Context ctxs -> AuthCheck v

instance AreAuths '[] ctxs v where
  runAuths _ _ = mempty

instance
  ( AuthCheck v ~ App (AuthArgs a) (Unapp (AuthArgs a) (AuthCheck v))
  , IsAuth a v
  , AreAuths as ctxs v
  , AppCtx ctxs (AuthArgs a) (Unapp (AuthArgs a) (AuthCheck v))
  ) =>
  AreAuths (a ': as) ctxs v
  where
  runAuths _ ctxs = go <> runAuths (Proxy :: Proxy as) ctxs
    where
      go =
        appCtx
          (Proxy :: Proxy (AuthArgs a))
          ctxs
          (runAuth (Proxy :: Proxy a) (Proxy :: Proxy v))

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
