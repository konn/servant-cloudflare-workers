{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Cloudflare.Workers.Internal.RouteResult (
  RouteResult (..),
) where

import Control.Monad (ap)
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
