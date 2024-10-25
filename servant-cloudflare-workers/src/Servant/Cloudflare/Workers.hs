{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- | This module lets you implement 'Worker's for defined APIs. You'll
most likely just need 'serve'.
-}
module Servant.Cloudflare.Workers (
  -- * Run a wai application from an API
  compileWorkerWithContext,
  compileWorker,
  serve,
  serveWithContext,
  serveWithContextT,
  WorkerContext,
  JSHandlers,
  JSObject (..),

  -- * Construct a wai Application from an API
  toFetchHandler,

  -- * Handlers for all standard combinators
  HasWorker (..),
  Worker,
  EmptyServer,
  emptyServer,
  Handler (..),
  runHandler,
  earlyReturn,
  serverError,
  getEnv,
  getSecret,
  getBinding,

  -- * Debugging the server layout
  layout,
  layoutWithContext,

  -- * Enter / hoisting server
  hoistWorker,

  -- ** Functions based on <https://hackage.haskell.org/package/mmorph mmorph>
  tweakResponse,

  -- * Context
  Context (..),
  HasContextEntry (getContextEntry),
  type (.++),
  (.++),

  -- ** NamedContext
  NamedContext (..),
  descendIntoNamedContext,

  -- * Basic Authentication
  BasicAuthCheck (BasicAuthCheck, unBasicAuthCheck),
  BasicAuthResult (..),

  -- * General Authentication

  -- , AuthHandler(unAuthHandler)
  -- , AuthServerData
  -- , mkAuthHandler

  -- * Default error type
  ServerError (..),

  -- ** 3XX
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

  -- * Formatting of errors from combinators

  --

  -- | You can configure how Servant will render errors that occur while parsing the request.
  ErrorFormatter,
  NotFoundErrorFormatter,
  ErrorFormatters,
  bodyParserErrorFormatter,
  urlParseErrorFormatter,
  headerParseErrorFormatter,
  notFoundErrorFormatter,
  DefaultErrorFormatters,
  defaultErrorFormatters,
  getAcceptHeader,

  -- * Re-exports
  FetchHandler,
  Tagged (..),
) where

import Control.Exception.Safe (displayException, tryAny)
import Data.Proxy (
  Proxy (..),
 )
import Data.Tagged (
  Tagged (..),
 )
import Data.Text (
  Text,
 )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Cloudflare.Worker.Handler
import Network.Cloudflare.Worker.Handler.Fetch
import Servant.Cloudflare.Workers.Internal
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)

-- * Implementing Servers

{- | Constraints that need to be satisfied on a context for it to be passed to 'serveWithContext'.

Typically, this will add default context entries to the context. You shouldn't typically
need to worry about these constraints, but if you write a helper function that wraps
'serveWithContext', you might need to include this constraint.
-}
type WorkerContext context =
  ( HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  )

compileWorkerWithContext ::
  forall e api ctx.
  (HasWorker e api ctx, WorkerContext ctx) =>
  (JSObject e -> FetchContext -> IO (Context ctx)) ->
  Worker e api ->
  IO JSHandlers
compileWorkerWithContext ctx act =
  toJSHandlers
    Handlers
      { fetch = \req env fctx -> do
          ectx <- tryAny $ ctx env fctx
          case ectx of
            Right workCtx -> serveWithContext @e @api Proxy Proxy workCtx act req env fctx
            Left exc ->
              toWorkerResponse $ responseServerError err500 {errBody = "Internal server error: " <> TE.encodeUtf8 (T.pack $ displayException exc)}
      }

compileWorker :: forall e api. (HasWorker e api '[]) => Worker e api -> IO JSHandlers
compileWorker = compileWorkerWithContext @e @api (const $ const $ pure EmptyContext)

{- | 'serve' allows you to implement an API and produce a wai 'Application'.

Example:

> type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
>         :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- POST /books
>
> server :: Worker MyApi
> server = listAllBooks :<|> postBook
>   where listAllBooks = ...
>         postBook book = ...
>
> myApi :: Proxy MyApi
> myApi = Proxy
>
> app :: Application
> app = serve myApi server
>
> main :: IO ()
> main = Network.Wai.Handler.Warp.run 8080 app
-}
serve :: (HasWorker e api '[]) => Proxy e -> Proxy api -> Worker e api -> FetchHandler e
serve pe p = serveWithContext pe p EmptyContext

{- | Like 'serve', but allows you to pass custom context.

'defaultErrorFormatters' will always be appended to the end of the passed context,
but if you pass your own formatter, it will override the default one.
-}
serveWithContext ::
  ( HasWorker e api context
  , WorkerContext context
  ) =>
  Proxy e ->
  Proxy api ->
  Context context ->
  Worker e api ->
  FetchHandler e
serveWithContext pe p context = serveWithContextT pe p context id

{- | A general 'serve' function that allows you to pass a custom context and hoisting function to
apply on all routes.
-}
serveWithContextT ::
  forall e api context m.
  (HasWorker e api context, WorkerContext context) =>
  Proxy e ->
  Proxy api ->
  Context context ->
  (forall x. m x -> Handler e x) ->
  WorkerT e api m ->
  FetchHandler e
serveWithContextT pe p context toHandler server =
  toFetchHandler (runRouter format404 (route pe p context (emptyDelayed router)))
  where
    router = Route $ hoistWorkerWithContext pe p (Proxy :: Proxy context) toHandler server
    format404 = notFoundErrorFormatter . getContextEntry . mkContextWithErrorFormatter $ context

{- | Hoist server implementation.

Sometimes our cherished `Handler` monad isn't quite the type you'd like for
your handlers. Maybe you want to thread some configuration in a @Reader@
monad. Or have your types ensure that your handlers don't do any IO. Use
`hoistWorker` (a successor of now deprecated @enter@).

With `hoistWorker`, you can provide a function,
to convert any number of endpoints from one type constructor to
another. For example

/Note:/ 'Worker' 'Raw' can also be entered. It will be retagged.

>>> import Control.Monad.Reader
>>> type ReaderAPI = "ep1" :> Get '[JSON] Int :<|> "ep2" :> Get '[JSON] String :<|> Raw :<|> EmptyAPI
>>> let readerApi = Proxy :: Proxy ReaderAPI
>>> let readerServer = return 1797 :<|> ask :<|> Tagged (error "raw server") :<|> emptyServer :: WorkerT ReaderAPI (Reader String)
>>> let nt x = return (runReader x "hi")
>>> let mainServer = hoistWorker readerApi nt readerServer :: Worker ReaderAPI
-}
hoistWorker ::
  (HasWorker e api '[]) =>
  Proxy e ->
  Proxy api ->
  (forall x. m x -> n x) ->
  WorkerT e api m ->
  WorkerT e api n
hoistWorker pe p = hoistWorkerWithContext pe p (Proxy :: Proxy '[])

{- | The function 'layout' produces a textual description of the internal
router layout for debugging purposes. Note that the router layout is
determined just by the API, not by the handlers.

Example:

For the following API

> type API =
>        "a" :> "d" :> Get '[JSON] NoContent
>   :<|> "b" :> Capture "x" Int :> Get '[JSON] Bool
>   :<|> "c" :> Put '[JSON] Bool
>   :<|> "a" :> "e" :> Get '[JSON] Int
>   :<|> "b" :> Capture "x" Int :> Put '[JSON] Bool
>   :<|> Raw

we get the following output:

> /
> ├─ a/
> │  ├─ d/
> │  │  └─•
> │  └─ e/
> │     └─•
> ├─ b/
> │  └─ <x::Int>/
> │     ├─•
> │     ┆
> │     └─•
> ├─ c/
> │  └─•
> ┆
> └─ <raw>

Explanation of symbols:

[@├@] Normal lines reflect static branching via a table.

[@a/@] Nodes reflect static path components.

[@─•@] Leaves reflect endpoints.

[@\<x::Int\>/@] This is a delayed capture of a single
path component named @x@, of expected type @Int@.

[@\<raw\>@] This is a part of the API we do not know anything about.

[@┆@] Dashed lines suggest a dynamic choice between the part above
and below. If there is a success for fatal failure in the first part,
that one takes precedence. If both parts fail, the \"better\" error
code will be returned.
-}
layout :: (HasWorker e api '[]) => Proxy e -> Proxy api -> Text
layout pe p = layoutWithContext pe p EmptyContext

-- | Variant of 'layout' that takes an additional 'Context'.
layoutWithContext ::
  (HasWorker e api context) =>
  Proxy e ->
  Proxy api ->
  Context context ->
  Text
layoutWithContext pe p context =
  routerLayout (route pe p context (emptyDelayed (FailFatal err501)))

{- $setup
>>> :set -XDataKinds
>>> :set -XTypeOperators
>>> import Servant.API
>>> import Servant.Cloudflare.Workers
-}
