{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Cloudflare.Workers.Internal.RouteResult (
  RouteResult (..),
  RouteResultT (..),
) where

import Control.Monad (
  ap,
 )
import Control.Monad.Catch (
  MonadThrow (..),
 )
import Control.Monad.IO.Unlift
import Control.Monad.Trans (
  MonadTrans (..),
 )
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Servant.Cloudflare.Workers.Internal.ServerError

-- | The result of matching against a path in the route tree.
data RouteResult a
  = -- | Keep trying other paths.
    --   The 'ServantError' should only be 404, 405 or 406.
    Fail ServerError
  | -- | Don't try other paths.
    FailFatal !ServerError
  | Route !a
  | FastReturn WorkerResponse
  deriving (Functor)

instance Applicative RouteResult where
  pure = Route
  (<*>) = ap

instance Monad RouteResult where
  return = pure
  Route a >>= f = f a
  Fail e >>= _ = Fail e
  FailFatal e >>= _ = FailFatal e
  FastReturn r >>= _ = FastReturn r

newtype RouteResultT m a = RouteResultT {runRouteResultT :: m (RouteResult a)}
  deriving (Functor)

instance MonadTrans RouteResultT where
  lift = RouteResultT . fmap Route

instance (Functor m, Monad m) => Applicative (RouteResultT m) where
  pure = RouteResultT . return . Route
  (<*>) = ap

instance (Monad m) => Monad (RouteResultT m) where
  return = pure
  m >>= k = RouteResultT $ do
    a <- runRouteResultT m
    case a of
      FastReturn r -> return $ FastReturn r
      Fail e -> return $ Fail e
      FailFatal e -> return $ FailFatal e
      Route b -> runRouteResultT (k b)

instance (MonadIO m) => MonadIO (RouteResultT m) where
  liftIO = lift . liftIO

instance (MonadThrow m) => MonadThrow (RouteResultT m) where
  throwM = lift . throwM
