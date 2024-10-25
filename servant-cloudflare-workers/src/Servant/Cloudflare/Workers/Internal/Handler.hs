{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Cloudflare.Workers.Internal.Handler (
  Handler (..),
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
  getRemainingPathPieces,
) where

import Control.Monad.Base (
  MonadBase (..),
 )
import Control.Monad.Catch (
  MonadCatch,
  MonadMask,
  MonadThrow,
 )
import Control.Monad.Error.Class (
  MonadError (..),
  throwError,
 )
import Control.Monad.IO.Class (
  MonadIO,
 )
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans.Control (
  MonadBaseControl (..),
 )
import Control.Monad.Trans.Except (
  ExceptT (..),
  runExceptT,
 )
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer.Strict (WriterT, runWriterT, tell)
import qualified Data.Aeson as J
import Data.Monoid (Ap (..))
import Data.String (
  fromString,
 )
import qualified Data.Text as T
import GHC.Generics (
  Generic,
 )
import GHC.TypeLits (KnownSymbol)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding (BindingsClass, ListMember)
import qualified Network.Cloudflare.Worker.Binding as Bindings
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Servant.Cloudflare.Workers.Internal.Response
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest (..))
import Servant.Cloudflare.Workers.Internal.ServerError (
  ServerError,
  err500,
  errBody,
  responseServerError,
 )

data HandlerEnv e = HandlerEnv
  { bindings :: !(JSObject e)
  , fetchContext :: !FetchContext
  , request :: !RoutingRequest
  }
  deriving (Generic)

type Finaliser = WorkerResponse -> Ap IO ()

newtype Handler e a = Handler {runHandler' :: WriterT Finaliser (ExceptT ServerReturn (ReaderT (HandlerEnv e) IO)) a}
  deriving stock (Generic)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (HandlerEnv e)
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadError ServerError (Handler e) where
  throwError = Handler . throwError . Error
  catchError (Handler m) f =
    Handler $
      catchError
        m
        ( \case
            Error err -> runHandler' $ f err
            l -> throwError l
        )

data ServerReturn
  = Error ServerError
  | Response RoutingResponse
  deriving (Generic)

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

instance MonadBase IO (Handler e) where
  liftBase = Handler . liftBase

instance MonadBaseControl IO (Handler e) where
  type StM (Handler e) a = Either ServerReturn (a, WorkerResponse -> Ap IO ())

  -- liftBaseWith :: (RunInBase Handler IO -> IO a) -> Handler a
  liftBaseWith f = Handler (liftBaseWith (\g -> f (g . runHandler')))

  -- restoreM :: StM Handler a -> Handler a
  restoreM st = Handler (restoreM st)

runHandler :: RoutingRequest -> JSObject e -> FetchContext -> Handler e a -> IO (Either ServerReturn (a, WorkerResponse -> Ap IO ()))
runHandler request bindings fetchContext = flip runReaderT HandlerEnv {..} . runExceptT . runWriterT . runHandler'

addFinaliser :: (WorkerResponse -> IO ()) -> Handler e ()
addFinaliser f = Handler $ tell $ Ap . f

getWorkerEnv :: Handler e (JSObject e)
getWorkerEnv = asks bindings

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
earlyReturn = Handler . throwError . Response

serverError :: forall e a. ServerError -> Handler e a
serverError = Handler . throwError . Error
