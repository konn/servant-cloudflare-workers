{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Effectful.Servant.Cloudflare.Workers (
  ServantWorker,
  compileWorker,
  genericCompileWorker,
  compileWorkerWithContext,
  genericCompileWorkerWithContext,
  runWorker,
  runWorkerWithContext,
  HasUniqueWorkerWith,
  HasUniqueWorker,

  -- * Interaction with the worker environment
  getEnvRaw,
  getEnv,
  getSecret,
  getBinding,
  withBinding,
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
  Context (..),
  AsWorkerT,
  AsWorker,
  B.Member,
  B.ListMember,
  B.Lookup',
  JSObject (..),
  Proxy (..),
  Handler (..),
  RoutingRequest (..),
  RoutingResponse (..),
  WorkerRequest,
  ServerReturn (..),
  ServerError (..),
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
import Data.Kind (Constraint)
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
import GHC.TypeError
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
import Servant.Cloudflare.Workers.Generic (AsWorker, AsWorkerT, genericWorkerT)
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
  GenericServant,
  Handler (Handler),
  HasWorker (WorkerT),
  JSHandlers,
  Proxy (..),
  ServerError (errBody),
  ToServant,
  ToServantApi,
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

genericCompileWorker ::
  forall e routes.
  ( HasWorker e (ToServantApi routes) '[]
  , GenericServant routes (AsWorkerT e (Eff '[ServantWorker e, IOE]))
  , ToServant routes (AsWorkerT e (Eff [ServantWorker e, IOE]))
      ~ WorkerT e (ToServantApi routes) (Eff '[ServantWorker e, IOE])
  ) =>
  routes (AsWorkerT e (Eff '[ServantWorker e, IOE])) ->
  IO JSHandlers
genericCompileWorker = compileWorker @e @(ToServantApi routes) . genericWorkerT @e @routes

compileWorkerWithContext ::
  forall e api ctx.
  (HasWorker e api ctx, WorkerContext ctx) =>
  (JSObject e -> FetchContext -> IO (Context ctx)) ->
  WorkerT e api (Eff '[ServantWorker e, IOE]) ->
  IO JSHandlers
compileWorkerWithContext ctx act = runEff $ unsafeEff \es ->
  toJSHandlers
    Handlers
      { fetch = \req env fctx -> do
          workCtx <- ctx env fctx
          runWorkerWithContext @e @api es workCtx act req env fctx
      }

genericCompileWorkerWithContext ::
  forall e routes ctx.
  ( HasWorker e (ToServantApi routes) ctx
  , WorkerContext ctx
  , GenericServant routes (AsWorkerT e (Eff '[ServantWorker e, IOE]))
  , ToServant routes (AsWorkerT e (Eff [ServantWorker e, IOE]))
      ~ WorkerT e (ToServantApi routes) (Eff '[ServantWorker e, IOE])
  ) =>
  (JSObject e -> FetchContext -> IO (Context ctx)) ->
  routes (AsWorkerT e (Eff '[ServantWorker e, IOE])) ->
  IO JSHandlers
genericCompileWorkerWithContext ctx = compileWorkerWithContext @e @(ToServantApi routes) ctx . genericWorkerT @e @routes

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

earlyReturn :: forall e es a. (HasUniqueWorkerWith e es) => RoutingResponse -> Eff es a
earlyReturn resp = do
  rep <- getStaticRep @(ServantWorker e)
  throwIO $ EarlyReturn rep.returnId $ Response resp

serverError :: forall e es a. (HasUniqueWorkerWith e es) => ServerError -> Eff es a
serverError err = do
  rep <- getStaticRep @(ServantWorker e)
  throwIO $ EarlyReturn rep.returnId $ Error err

addFinaliser :: forall e es. (HasUniqueWorkerWith e es) => (WorkerResponse -> IO ()) -> Eff es ()
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

getFetchContext :: forall e es. (HasUniqueWorkerWith e es) => Eff es FetchContext
getFetchContext = do
  rep <- getStaticRep @(ServantWorker e)
  pure rep.fetchContext

getBindings :: forall e es. (HasUniqueWorkerWith e es) => Eff es (JSObject e)
getBindings = do
  rep <- getStaticRep @(ServantWorker e)
  pure rep.workerEnv

getRoutingRequest :: forall e es. (HasUniqueWorkerWith e es) => Eff es RoutingRequest
getRoutingRequest = do
  rep <- getStaticRep @(ServantWorker e)
  pure rep.request

getRawRequest :: forall e es. (HasUniqueWorkerWith e es) => Eff es WorkerRequest
getRawRequest = do
  req <- getRoutingRequest @e
  pure req.rawRequest

getEnvRaw ::
  forall vs ss bs es.
  forall l ->
  ( KnownSymbol l
  , HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , B.ListMember l vs
  ) =>
  Eff es Value
getEnvRaw l = B.getEnv l <$> getBindings @(BindingsClass vs ss bs)

getEnv ::
  forall vs ss bs a es.
  forall l ->
  ( KnownSymbol l
  , HasUniqueWorkerWith (BindingsClass vs ss bs) es
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
  , HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , B.ListMember l ss
  ) =>
  Eff es T.Text
getSecret l = B.getSecret l <$> getBindings @(BindingsClass vs ss bs)

getBinding ::
  forall vs ss bs es a.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , B.Member l bs
  , B.Lookup' l bs ~ a
  ) =>
  Eff es (JSObject a)
getBinding l = B.getBinding l <$> getBindings @(BindingsClass vs ss bs)

withBinding ::
  forall vs ss bs es a x.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , B.Member l bs
  , B.Lookup' l bs ~ x
  ) =>
  (JSObject x -> Eff es a) ->
  Eff es a
withBinding l = (=<< getBinding @vs @ss @bs l)

type HasUniqueWorker :: [Effect] -> Constraint
class (ServantWorker (WorkerEnvOf es) ∈ es) => HasUniqueWorker es

type WorkerEnvOf es = WorkerEnvAux es es

type family WorkerEnvAux (super :: [Effect]) (es :: [Effect]) where
  WorkerEnvAux _ (ServantWorker e ': es) = e
  WorkerEnvAux super (_ ': es) = WorkerEnvAux super es
  WorkerEnvAux super '[] = TypeError ('Text "No ServantWorker found in a stack: " ':<>: 'ShowType super)

type NoWorker es = NoWorkerAux es es

type NoWorkerAux :: [Effect] -> [Effect] -> Constraint
type family NoWorkerAux super cs where
  NoWorkerAux super '[] = ()
  NoWorkerAux super (ServantWorker e ': cs) = TypeError ('Text "No ServantWorker must be present in the stack but got: " ':<>: 'ShowType super)
  NoWorkerAux super (_ ': cs) = NoWorkerAux super cs

instance
  ( Unsatisfiable ('Text "Exactly one worker constraint expected")
  ) =>
  HasUniqueWorker '[]

instance
  {-# OVERLAPPING #-}
  ( NoWorker es
  , WorkerEnvOf (ServantWorker e ': es) ~ e
  ) =>
  HasUniqueWorker (ServantWorker e ': es)

instance
  {-# OVERLAPPABLE #-}
  ( HasUniqueWorker es
  , WorkerEnvOf (e ': es) ~ WorkerEnvOf es
  ) =>
  HasUniqueWorker (e ': es)

type HasUniqueWorkerWith e es = (WorkerEnvOf es ~ e, HasUniqueWorker es)
