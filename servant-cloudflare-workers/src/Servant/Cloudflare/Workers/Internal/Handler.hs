{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Servant.Cloudflare.Workers.Internal.Handler (
  Handler (),
  Finaliser,
  addFinaliser,
  runHandler,
  earlyReturn,
  serverError,
  ServerReturn (..),
  HandlerEnv (..),
  responseServerReturn,
  getEnv,
  getBinding,
  getSecret,
  getRawRequest,
  getWorkerEnv,
  getFetchContext,
  waitUntil,
  waitUntil',
  getRemainingPathPieces,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (evaluate)
import Control.Exception.Safe (Exception, throwM)
import qualified Control.Exception.Safe as Safe
import Control.Monad (forM)
import Control.Monad.Catch (
  MonadCatch,
  MonadMask,
  MonadThrow,
 )
import qualified Control.Monad.Catch as Unsafe
import Control.Monad.Error.Class (
  MonadError (..),
  throwError,
 )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Aeson as J
import qualified Data.Bifunctor as Bi
import Data.Monoid (Ap (..))
import Data.String (fromString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding (BindingsClass, ListMember)
import qualified Network.Cloudflare.Worker.Binding as Bindings
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import qualified Network.Cloudflare.Worker.Handler.Fetch as FC
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Servant.Cloudflare.Workers.Internal.Response
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest (..))
import Servant.Cloudflare.Workers.Internal.ServerError (ServerError, err500, errBody, responseServerError)

data HandlerEnv e = HandlerEnv
  { bindings :: !(JSObject e)
  , fetchContext :: !FetchContext
  , request :: !RoutingRequest
  }
  deriving (Generic)

data HandlerRep e = HandlerRep
  { finaliser :: !(MVar Finaliser)
  , environment :: !(HandlerEnv e)
  }

type Finaliser = WorkerResponse -> Ap IO ()

newtype Handler e a = Handler {runHandler' :: ReaderT (HandlerRep e) IO a}
  deriving stock (Generic)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadReader (HandlerEnv e) (Handler e) where
  ask = Handler $ asks environment
  local f (Handler m) = Handler $ local (\e -> e {environment = f e.environment}) m

instance MonadError ServerError (Handler e) where
  throwError = Handler . throwM . ServereReturnExc . Error
  catchError (Handler m) f =
    Handler $
      Unsafe.catch
        m
        ( \case
            ServereReturnExc (Error err) -> runHandler' $ f err
            l -> Unsafe.throwM l
        )

data ServerReturn
  = Error ServerError
  | Response RoutingResponse
  deriving (Generic)

newtype ServereReturnExc = ServereReturnExc {unServerReturn :: ServerReturn}
  deriving (Generic)
  deriving anyclass (Exception)

instance Show ServereReturnExc where
  show (ServereReturnExc (Error e)) = "Error " <> show e
  show (ServereReturnExc (Response _)) = "EarlyReturnResponse (..)"

getRawRequest :: Handler e WorkerRequest
getRawRequest = asks $ (.rawRequest) . request

getRemainingPathPieces :: Handler e [T.Text]
getRemainingPathPieces = asks $ pathInfo . request

responseServerReturn :: ServerReturn -> IO WorkerResponse
responseServerReturn (Error err) =
  toWorkerResponse $ responseServerError err
responseServerReturn (Response rsp) =
  toWorkerResponse rsp

instance Show ServerReturn where
  showsPrec d (Error e) = showParen (d > 10) $ showString "Error " . showsPrec 11 e
  showsPrec d (Response _) = showParen (d > 10) $ showString "Response (..)"

instance MonadFail (Handler e) where
  fail str = throwError err500 {errBody = fromString str}

runHandler :: RoutingRequest -> JSObject e -> FetchContext -> Handler e a -> IO (Either ServerReturn (a, WorkerResponse -> Ap IO ()))
runHandler request bindings fetchContext (Handler act) = Safe.mask \restore -> do
  rep <- restore $ do
    finaliser <- newMVar $! mempty
    let environment = HandlerEnv {..}
    pure HandlerRep {..}
  resl <- Unsafe.try $ runReaderT act rep
  restore $ forM (Bi.first unServerReturn resl) \a ->
    (a,) <$> readMVar (finaliser rep)

addFinaliser :: (WorkerResponse -> IO ()) -> Handler e ()
addFinaliser f = Handler do
  fin <- asks finaliser
  liftIO $ modifyMVar_ fin \old -> evaluate $ old <> (Ap . f)

getWorkerEnv :: Handler e (JSObject e)
getWorkerEnv = asks bindings

getFetchContext :: Handler e FetchContext
getFetchContext = asks (.fetchContext)

waitUntil :: Promise a -> Handler e ()
waitUntil p = do
  ctx <- getFetchContext
  liftIO $ FC.waitUntil ctx p

waitUntil' :: Promised a b -> Handler e ()
waitUntil' p = do
  ctx <- getFetchContext
  liftIO $ FC.waitUntil ctx $ jsPromise p

getEnv ::
  forall l ->
  forall es ss bs.
  (ListMember l es, KnownSymbol l) =>
  Handler (BindingsClass es ss bs) J.Value
getEnv l = asks $ Bindings.getEnv l . bindings

getSecret ::
  forall l ->
  forall es ss bs.
  (ListMember l ss, KnownSymbol l) =>
  Handler (BindingsClass es ss bs) T.Text
getSecret l = asks $ Bindings.getSecret l . bindings

getBinding ::
  forall l ->
  forall es ss bs x.
  (Member l bs, x ~ Lookup' l bs) =>
  Handler (BindingsClass es ss bs) (JSObject x)
getBinding l = asks $ Bindings.getBinding l . bindings

earlyReturn :: RoutingResponse -> Handler e a
earlyReturn = Handler . throwM . ServereReturnExc . Response

serverError :: forall e a. ServerError -> Handler e a
serverError = Handler . throwM . ServereReturnExc . Error
