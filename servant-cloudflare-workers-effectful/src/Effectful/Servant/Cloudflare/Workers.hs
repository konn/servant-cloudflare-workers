{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Effectful.Servant.Cloudflare.Workers (
  ServantWorker,
  compileWorker,
  compileWorkerWithContext,
  runWorker,
  runWorkerWithContext,

  -- * Interaction with the worker environment
  getEnvRaw,
  getEnv,
  getSecret,
  getBinding,
  getFetchContext,
  getBindings,
  getRoutingRequest,
  getRawRequest,
  earlyReturn,
  serverError,

  -- * Type synonyms to avoid collisions
  type (/>),
  type (∈),

  -- * Low-level combinators
  addFinaliser,
  interpretWorker,

  -- * Re-exports
  JSObject (..),
  Proxy (..),
  Handler (..),
  RoutingRequest (..),
  RoutingResponse (..),
  WorkerRequest,
  ServerReturn (..),
  err300,
  err301,
  err302,
  err303,
  err304,
  err305,
  err307,

  -- ** 4XX
  err400,
  err401,
  err402,
  err403,
  err404,
  err405,
  err406,
  err407,
  err409,
  err410,
  err411,
  err412,
  err413,
  err414,
  err415,
  err416,
  err417,
  err418,
  err422,
  err429,

  -- ** 5XX
  err500,
  err501,
  err502,
  err503,
  err504,
  err505,

  -- ** Effectful
  Eff,
) where

import Control.Exception.Safe (Exception, displayException, fromException, handleAny, throwIO, throwString)
import Control.Monad.Error.Class qualified as MTL
import Control.Monad.Reader.Class qualified as MTL
import Control.Monad.Trans.Writer.Strict qualified as MTL
import Data.Aeson (FromJSON, Value, fromJSON)
import Data.Aeson qualified as A
import Data.Monoid (Ap (..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Unique qualified as DU
import Effectful hiding (type (:>))
import Effectful qualified
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive (Env, cloneEnv)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import GHC.Wasm.Object.Builtins (JSObject (..), Prototype)
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding qualified as B
import Network.Cloudflare.Worker.Handler (Handlers (..), toJSHandlers)
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Network.Cloudflare.Worker.Request (WorkerRequest)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Servant.API qualified as Servant
import Servant.Cloudflare.Workers qualified as Servant
import Servant.Cloudflare.Workers.Internal.Handler (
  Finaliser,
  HandlerEnv (..),
  ServerReturn (..),
 )
import Servant.Cloudflare.Workers.Internal.Response (RoutingResponse (..))
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest (..))
import Servant.Cloudflare.Workers.Prelude (
  Context (EmptyContext),
  FetchHandler,
  Handler (Handler),
  HasWorker (WorkerT),
  JSHandlers,
  Proxy (..),
  ServerError (errBody),
  WorkerContext,
  err300,
  err301,
  err302,
  err303,
  err304,
  err305,
  err307,
  err400,
  err401,
  err402,
  err403,
  err404,
  err405,
  err406,
  err407,
  err409,
  err410,
  err411,
  err412,
  err413,
  err414,
  err415,
  err416,
  err417,
  err418,
  err422,
  err429,
  err500,
  err501,
  err502,
  err503,
  err504,
  err505,
 )

type (/>) = (Servant.:>)

type (∈) = (Effectful.:>)

compileWorker ::
  forall e api.
  (HasWorker e api '[]) =>
  WorkerT e api (Eff '[ServantWorker e, IOE]) ->
  IO JSHandlers
compileWorker act = toJSHandlers Handlers {fetch = runWorker @e @api act}

compileWorkerWithContext ::
  forall e api ctx.
  (HasWorker e api ctx, WorkerContext ctx) =>
  Context ctx ->
  WorkerT e api (Eff '[ServantWorker e, IOE]) ->
  IO JSHandlers
compileWorkerWithContext ctx act = runEff $ unsafeEff \es ->
  toJSHandlers Handlers {fetch = runWorkerWithContext @e @api es ctx act}

runWorker ::
  forall e api.
  (HasWorker e api '[]) =>
  WorkerT e api (Eff '[ServantWorker e, IOE]) ->
  FetchHandler e
runWorker act req env fctx = runEff $ unsafeEff \es ->
  runWorkerWithContext @e @api es EmptyContext act req env fctx

runWorkerWithContext ::
  forall e api context es.
  ( HasWorker e api context
  , WorkerContext context
  , IOE ∈ es
  ) =>
  Env es ->
  Context context ->
  WorkerT e api (Eff (ServantWorker e ': es)) ->
  FetchHandler e
runWorkerWithContext env ctx w =
  Servant.serveWithContextT (Proxy @e) (Proxy @api) ctx (interpretWorker env) w

data ServantWorker (e :: Prototype) :: Effect

type instance DispatchOf (ServantWorker e) = Static WithSideEffects

data instance StaticRep (ServantWorker e)
  = ServantWorkerRep
  { workerEnv :: !(JSObject e)
  , fetchContext :: !FetchContext
  , request :: !RoutingRequest
  , finaliser :: !Finaliser
  , returnId :: !ReturnId
  }

newtype ReturnId = ReturnId {retId :: DU.Unique}
  deriving (Eq, Ord)

earlyReturn :: forall e es a. (ServantWorker e ∈ es) => RoutingResponse -> Eff es a
earlyReturn resp = do
  rep <- getStaticRep @(ServantWorker e)
  throwIO $ EarlyReturn rep.returnId $ Response resp

serverError :: forall e es a. (ServantWorker e ∈ es) => ServerError -> Eff es a
serverError err = do
  rep <- getStaticRep @(ServantWorker e)
  throwIO $ EarlyReturn rep.returnId $ Error err

addFinaliser :: forall e es. (ServantWorker e ∈ es) => (WorkerResponse -> IO ()) -> Eff es ()
addFinaliser fin = stateStaticRep @(ServantWorker e) $ \rep ->
  ((), rep {finaliser = rep.finaliser <> (Ap . fin)})

data EarlyReturn = EarlyReturn !ReturnId !ServerReturn
  deriving (Generic)
  deriving anyclass (Exception)

instance Show EarlyReturn where
  showsPrec d (EarlyReturn uid a) =
    showParen (d > 10) $
      showString "EarlyReturn " . shows (DU.hashUnique uid.retId) . showChar ' ' . showsPrec 11 a

handleEarlyReturn :: forall e es a. (ServantWorker e ∈ es) => Eff es a -> Eff es (Either ServerReturn a)
handleEarlyReturn act = do
  rep <- getStaticRep @(ServantWorker e)
  handleAny
    ( \exc ->
        unsafeEff_ $
          case fromException exc of
            Just exc'@(EarlyReturn uid cast)
              | uid == rep.returnId -> pure $ Left cast
              | otherwise -> throwIO exc'
            Nothing ->
              pure $
                Left $
                  Error $
                    err500
                      { errBody = "Internal server error: " <> LTE.encodeUtf8 (LT.pack $ displayException exc)
                      }
    )
    (Right <$> act)

runServantWorker ::
  forall e es a.
  (IOE ∈ es) =>
  RoutingRequest ->
  JSObject e ->
  FetchContext ->
  Eff (ServantWorker e ': es) a ->
  Eff es (Either ServerReturn (a, Finaliser))
runServantWorker req env fctx act = do
  uniq <- liftIO DU.newUnique
  let rep = ServantWorkerRep {workerEnv = env, fetchContext = fctx, request = req, finaliser = mempty, returnId = ReturnId uniq}
  (v, rep') <- runStaticRep @(ServantWorker e) rep $ handleEarlyReturn @e act
  pure $ (,rep'.finaliser) <$> v

interpretWorker ::
  (IOE ∈ es) =>
  Env es ->
  Eff (ServantWorker e : es) a ->
  Handler e a
interpretWorker env act = do
  HandlerEnv {..} <- MTL.ask
  v <- liftIO do
    env' <- cloneEnv env
    unEff (runServantWorker request bindings fetchContext act) env'
  either (Handler . MTL.liftEither . Left) (Handler . MTL.WriterT . pure) v

getFetchContext :: forall e es. (ServantWorker e ∈ es) => Eff es FetchContext
getFetchContext = do
  rep <- getStaticRep @(ServantWorker e)
  pure rep.fetchContext

getBindings :: forall e es. (ServantWorker e ∈ es) => Eff es (JSObject e)
getBindings = do
  rep <- getStaticRep @(ServantWorker e)
  pure rep.workerEnv

getRoutingRequest :: forall e es. (ServantWorker e ∈ es) => Eff es RoutingRequest
getRoutingRequest = do
  rep <- getStaticRep @(ServantWorker e)
  pure rep.request

getRawRequest :: forall e es. (ServantWorker e ∈ es) => Eff es WorkerRequest
getRawRequest = do
  req <- getRoutingRequest @e
  pure req.rawRequest

getEnvRaw ::
  forall vs ss bs es.
  forall l ->
  ( KnownSymbol l
  , ServantWorker (BindingsClass vs ss bs) ∈ es
  , B.ListMember l vs
  ) =>
  Eff es Value
getEnvRaw l = B.getEnv l <$> getBindings @(BindingsClass vs ss bs)

getEnv ::
  forall vs ss bs a es.
  forall l ->
  ( KnownSymbol l
  , ServantWorker (BindingsClass vs ss bs) ∈ es
  , B.ListMember l vs
  , FromJSON a
  , HasCallStack
  ) =>
  Eff es a
getEnv l =
  either throwString pure . eitherResult . fromJSON
    =<< getEnvRaw @vs @ss @bs l

eitherResult :: A.Result a -> Either String a
eitherResult (A.Success a) = Right a
eitherResult (A.Error e) = Left e

getSecret ::
  forall vs ss bs es.
  forall l ->
  ( KnownSymbol l
  , ServantWorker (BindingsClass vs ss bs) ∈ es
  , B.ListMember l ss
  ) =>
  Eff es T.Text
getSecret l = B.getSecret l <$> getBindings @(BindingsClass vs ss bs)

getBinding ::
  forall vs ss bs es a.
  forall l ->
  ( ServantWorker (BindingsClass vs ss bs) ∈ es
  , B.Member l bs
  , B.Lookup' l bs ~ a
  ) =>
  Eff es (JSObject a)
getBinding l = B.getBinding l <$> getBindings @(BindingsClass vs ss bs)
