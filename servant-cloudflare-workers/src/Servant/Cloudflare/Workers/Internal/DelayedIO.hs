{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Cloudflare.Workers.Internal.DelayedIO (
  DelayedIO (),
  runDelayedIO,
  withRequest,
  delayedFail,
  delayedFailFatal,
  liftRouteResult,
) where

import Control.Arrow ((>>>))
import Control.Exception (handle, throwIO)
import Control.Monad.Catch (
  Exception,
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
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Servant.Cloudflare.Workers.Internal.Handler (HandlerEnv (..))
import Servant.Cloudflare.Workers.Internal.RouteResult
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest)
import Servant.Cloudflare.Workers.Internal.ServerError

{- | Computations used in a 'Delayed' can depend on the
incoming 'Request', may perform 'IO', and result in a
'RouteResult', meaning they can either succeed, fail
(with the possibility to recover), or fail fatally.
-}
newtype DelayedIO e a = DelayedIO {runDelayedIO' :: ReaderT (HandlerEnv e) IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (HandlerEnv e)
    , MonadThrow
    )

data RouteResultExc
  = FailExc ServerError
  | FailFatalExc ServerError
  | FastReturnExc !WorkerResponse
  deriving (Generic)

instance Show RouteResultExc where
  showsPrec d (FailExc e) = showParen (d > 10) $ showString "FailExc " . showsPrec 11 e
  showsPrec d (FailFatalExc e) =
    showParen (d > 10) $ showString "FailFatalExc " . showsPrec 11 e
  showsPrec d (FastReturnExc {}) = showParen (d > 10) $ showString "FastReturnExc <>"

instance Exception RouteResultExc

liftRouteResult :: RouteResult a -> DelayedIO e a
{-# INLINE liftRouteResult #-}
liftRouteResult x = DelayedIO $ lift $ case x of
  Fail e -> throwIO $ FailExc e
  FailFatal e -> throwIO $ FailFatalExc e
  FastReturn r -> throwIO $ FastReturnExc r
  Route a -> pure a

runDelayedIO :: DelayedIO e a -> RoutingRequest -> JSObject e -> FetchContext -> IO (RouteResult a)
runDelayedIO m request bindings fetchContext = catchRouteT $ runReaderT (runDelayedIO' m) HandlerEnv {..}

catchRouteT :: IO a -> IO (RouteResult a)
{-# INLINE catchRouteT #-}
catchRouteT =
  fmap Route >>> handle \case
    FailExc e -> pure $ Fail e
    FailFatalExc e -> pure $ FailFatal e
    FastReturnExc r -> pure $ FastReturn r

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
