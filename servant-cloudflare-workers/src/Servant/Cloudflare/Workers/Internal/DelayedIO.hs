{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Cloudflare.Workers.Internal.DelayedIO (
  DelayedIO (..),
  runDelayedIO,
  withRequest,
  delayedFail,
  delayedFailFatal,
  liftRouteResult,
) where

import Control.Monad.Catch (
  MonadThrow (..),
 )
import Control.Monad.IO.Unlift ()
import Control.Monad.Reader (
  MonadReader (..),
  ReaderT (..),
  runReaderT,
 )
import Control.Monad.Trans (
  MonadIO (..),
  MonadTrans (..),
 )
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Servant.Cloudflare.Workers.Internal.Handler (HandlerEnv (..))
import Servant.Cloudflare.Workers.Internal.RouteResult
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest)
import Servant.Cloudflare.Workers.Internal.ServerError

{- | Computations used in a 'Delayed' can depend on the
incoming 'Request', may perform 'IO', and result in a
'RouteResult', meaning they can either succeed, fail
(with the possibility to recover), or fail fatally.
-}
newtype DelayedIO e a = DelayedIO {runDelayedIO' :: ReaderT (HandlerEnv e) (RouteResultT IO) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (HandlerEnv e)
    , MonadThrow
    )

liftRouteResult :: RouteResult a -> DelayedIO e a
liftRouteResult x = DelayedIO $ lift $ RouteResultT . return $ x

runDelayedIO :: DelayedIO e a -> RoutingRequest -> JSObject e -> FetchContext -> IO (RouteResult a)
runDelayedIO m request bindings fetchContext = runRouteResultT $ runReaderT (runDelayedIO' m) HandlerEnv {..}

-- | Fail with the option to recover.
delayedFail :: ServerError -> DelayedIO e a
delayedFail err = liftRouteResult $ Fail err

-- | Fail fatally, i.e., without any option to recover.
delayedFailFatal :: ServerError -> DelayedIO e a
delayedFailFatal err = liftRouteResult $ FailFatal err

-- | Gain access to the incoming request.
withRequest :: (RoutingRequest -> JSObject e -> FetchContext -> DelayedIO e a) -> DelayedIO e a
withRequest f = do
  HandlerEnv {..} <- ask
  f request bindings fetchContext
