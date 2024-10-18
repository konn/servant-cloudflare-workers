{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Cloudflare.Workers.Internal.DelayedIO where

import Control.Monad.Base (
  MonadBase (..),
 )
import Control.Monad.Catch (
  MonadThrow (..),
 )
import Control.Monad.Reader (
  MonadReader (..),
  ReaderT (..),
  runReaderT,
 )
import Control.Monad.Trans (
  MonadIO (..),
  MonadTrans (..),
 )
import Control.Monad.Trans.Control (
  MonadBaseControl (..),
 )
import Control.Monad.Trans.Resource (
  MonadResource (..),
  ResourceT,
  runInternalState,
  transResourceT,
  withInternalState,
 )
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Servant.Cloudflare.Workers.Internal.Handler (HandlerEnv (..))
import Servant.Cloudflare.Workers.Internal.RouteResult
import Servant.Cloudflare.Workers.Internal.ServerError

{- | Computations used in a 'Delayed' can depend on the
incoming 'Request', may perform 'IO', and result in a
'RouteResult', meaning they can either succeed, fail
(with the possibility to recover), or fail fatally.
-}
newtype DelayedIO e a = DelayedIO {runDelayedIO' :: ReaderT (HandlerEnv e, WorkerRequest) (ResourceT (RouteResultT IO)) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (HandlerEnv e, WorkerRequest)
    , MonadThrow
    , MonadResource
    )

instance MonadBase IO (DelayedIO e) where
  liftBase = liftIO

liftRouteResult :: RouteResult a -> DelayedIO e a
liftRouteResult x = DelayedIO $ lift . lift $ RouteResultT . return $ x

instance MonadBaseControl IO (DelayedIO e) where
  -- type StM DelayedIO a = StM (ReaderT Request (ResourceT (RouteResultT IO))) a
  -- liftBaseWith f = DelayedIO $ liftBaseWith $ \g -> f (g . runDelayedIO')
  -- restoreM       = DelayedIO . restoreM

  type StM (DelayedIO e) a = RouteResult a
  liftBaseWith f = DelayedIO $ ReaderT $ \req -> withInternalState $ \s ->
    liftBaseWith $ \runInBase -> f $ \x ->
      runInBase (runInternalState (runReaderT (runDelayedIO' x) req) s)
  restoreM = DelayedIO . lift . withInternalState . const . restoreM

runDelayedIO :: DelayedIO e a -> WorkerRequest -> JSObject e -> FetchContext -> ResourceT IO (RouteResult a)
runDelayedIO m req bindings fetchContext = transResourceT runRouteResultT $ runReaderT (runDelayedIO' m) (HandlerEnv {..}, req)

-- | Fail with the option to recover.
delayedFail :: ServerError -> DelayedIO e a
delayedFail err = liftRouteResult $ Fail err

-- | Fail fatally, i.e., without any option to recover.
delayedFailFatal :: ServerError -> DelayedIO e a
delayedFailFatal err = liftRouteResult $ FailFatal err

-- | Gain access to the incoming request.
withRequest :: (WorkerRequest -> JSObject e -> FetchContext -> DelayedIO e a) -> DelayedIO e a
withRequest f = do
  (HandlerEnv {..}, req) <- ask
  f req bindings fetchContext
