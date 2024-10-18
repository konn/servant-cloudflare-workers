{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Cloudflare.Workers.Internal.Handler where

import Control.Monad.Base (
  MonadBase (..),
 )
import Control.Monad.Catch (
  MonadCatch,
  MonadMask,
  MonadThrow,
 )
import Control.Monad.Error.Class (
  MonadError,
  throwError,
 )
import Control.Monad.IO.Class (
  MonadIO,
 )
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Control (
  MonadBaseControl (..),
 )
import Control.Monad.Trans.Except (
  ExceptT (ExceptT),
  runExceptT,
 )
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.String (
  fromString,
 )
import GHC.Generics (
  Generic,
 )
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Servant.Cloudflare.Workers.Internal.ServerError (
  ServerError,
  err500,
  errBody,
 )

data HandlerEnv e = HandlerEnv
  { bindings :: !(JSObject e)
  , fetchContext :: !FetchContext
  }
  deriving (Generic)

newtype Handler e a = Handler {runHandler' :: ExceptT ServerError (ReaderT (HandlerEnv e) IO) a}
  deriving stock (Generic)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ServerError
    , MonadReader (HandlerEnv e)
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadFail (Handler e) where
  fail str = throwError err500 {errBody = fromString str}

instance MonadBase IO (Handler e) where
  liftBase = Handler . liftBase

instance MonadBaseControl IO (Handler e) where
  type StM (Handler e) a = Either ServerError a

  -- liftBaseWith :: (RunInBase Handler IO -> IO a) -> Handler a
  liftBaseWith f = Handler (liftBaseWith (\g -> f (g . runHandler')))

  -- restoreM :: StM Handler a -> Handler a
  restoreM st = Handler (restoreM st)

runHandler :: JSObject e -> FetchContext -> Handler e a -> IO (Either ServerError a)
runHandler bindings fetchContext = flip runReaderT HandlerEnv {..} . runExceptT . runHandler'
