{-# LANGUAGE CPP #-}

module Servant.Auth.Cloudflare.Workers.Internal.Types where

import Control.Applicative
import Control.Monad (MonadPlus (..), ap)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Reader
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest)

-- | The result of an authentication attempt.
data AuthResult val
  = BadPassword
  | NoSuchUser
  | -- | Authentication succeeded.
    Authenticated val
  | -- | If an authentication procedure cannot be carried out - if for example it
    -- expects a password and username in a header that is not present -
    -- @Indefinite@ is pureed. This indicates that other authentication
    -- methods should be tried.
    Indefinite
  deriving (Eq, Show, Read, Generic, Ord, Functor, Traversable, Foldable)

instance Semigroup (AuthResult val) where
  Indefinite <> y = y
  x <> _ = x

instance Monoid (AuthResult val) where
  mempty = Indefinite
  mappend = (<>)

instance Applicative AuthResult where
  pure = Authenticated
  (<*>) = ap

instance Monad AuthResult where
  Authenticated v >>= f = f v
  BadPassword >>= _ = BadPassword
  NoSuchUser >>= _ = NoSuchUser
  Indefinite >>= _ = Indefinite

instance Alternative AuthResult where
  empty = mzero
  (<|>) = mplus

instance MonadPlus AuthResult where
  mzero = mempty
  mplus = (<>)

{- | An @AuthCheck@ is the function used to decide the authentication status
(the 'AuthResult') of a request. Different @AuthCheck@s may be combined as a
Monoid or Alternative; the semantics of this is that the *first*
non-'Indefinite' result from left to right is used and the rest are ignored.
-}
newtype AuthCheck e val = AuthCheck
  {runAuthCheck :: RoutingRequest -> JSObject e -> FetchContext -> IO (AuthResult val)}
  deriving (Generic, Functor)

instance Semigroup (AuthCheck e val) where
  AuthCheck f <> AuthCheck g = AuthCheck $ \x y z -> do
    fx <- f x y z
    case fx of
      Indefinite -> g x y z
      r -> pure r

instance Monoid (AuthCheck e val) where
  mempty = AuthCheck $ const $ pure mempty
  mappend = (<>)

instance Applicative (AuthCheck e) where
  pure = AuthCheck . pure . pure . pure . pure . pure
  (<*>) = ap

instance Monad (AuthCheck e) where
  AuthCheck ac >>= f = AuthCheck $ \req env fctx -> do
    aresult <- ac req env fctx
    case aresult of
      Authenticated usr -> runAuthCheck (f usr) req env fctx
      BadPassword -> pure BadPassword
      NoSuchUser -> pure NoSuchUser
      Indefinite -> pure Indefinite

#if !MIN_VERSION_base(4,13,0)
  fail = Fail.fail
#endif

instance Fail.MonadFail (AuthCheck e) where
  fail _ = AuthCheck . const . const . const $ pure Indefinite

instance MonadReader (RoutingRequest, JSObject e, FetchContext) (AuthCheck e) where
  ask = AuthCheck $ \x y z -> pure (Authenticated (x, y, z))
  local f (AuthCheck check) = AuthCheck $ \req e fctx ->
    let (req', e', fctx') = f (req, e, fctx)
     in check req' e' fctx'

instance MonadIO (AuthCheck e) where
  liftIO action = AuthCheck $ const $ const $ const $ Authenticated <$> action

instance Alternative (AuthCheck e) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus (AuthCheck e) where
  mzero = mempty
  mplus = (<>)
