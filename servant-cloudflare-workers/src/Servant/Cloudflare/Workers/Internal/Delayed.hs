{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Cloudflare.Workers.Internal.Delayed (
  Delayed,
  runDelayed,
  emptyDelayed,
  addCapture,
  addParameterCheck,
  runAction,
  addMethodCheck,
  addAcceptCheck,
  addHeaderCheck,
  addBodyCheck,
  addAuthCheck,
  passToServer,
) where

import Control.Monad.IO.Class (
  MonadIO (..),
 )
import Control.Monad.Reader (
  ask,
 )
import Control.Monad.Trans.Resource (
  ResourceT,
  runResourceT,
 )
import Data.Monoid (Ap (..))
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Servant.Cloudflare.Workers.Internal.DelayedIO
import Servant.Cloudflare.Workers.Internal.Handler
import Servant.Cloudflare.Workers.Internal.Response
import Servant.Cloudflare.Workers.Internal.RouteResult
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest)
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)

{- | A 'Delayed' is a representation of a handler with scheduled
delayed checks that can trigger errors.

Why would we want to delay checks?

There are two reasons:

1. In a straight-forward implementation, the order in which we
perform checks will determine the error we generate. This is
because once an error occurs, we would abort and not perform
any subsequent checks, but rather return the current error.

This is not a necessity: we could continue doing other checks,
and choose the preferred error. However, that would in general
mean more checking, which leads us to the other reason.

2. We really want to avoid doing certain checks too early. For
example, captures involve parsing, and are much more costly
than static route matches. In particular, if several paths
contain the "same" capture, we'd like as much as possible to
avoid trying the same parse many times. Also tricky is the
request body. Again, this involves parsing, but also, WAI makes
obtaining the request body a side-effecting operation. We
could/can work around this by manually caching the request body,
but we'd rather keep the number of times we actually try to
decode the request body to an absolute minimum.

We prefer to have the following relative priorities of error
codes:

@
404
405 (bad method)
401 (unauthorized)
415 (unsupported media type)
406 (not acceptable)
400 (bad request)
@

Therefore, while routing, we delay most checks so that they
will ultimately occur in the right order.

A 'Delayed' contains many delayed blocks of tests, and
the actual handler:

1. Delayed captures. These can actually cause 404, and
while they're costly, they should be done first among the
delayed checks (at least as long as we do not decouple the
check order from the error reporting, see above). Delayed
captures can provide inputs to the actual handler.

2. Method check(s). This can cause a 405. On success,
it does not provide an input for the handler. Method checks
are comparatively cheap.

3. Authentication checks. This can cause 401.

4. Accept and content type header checks. These checks
can cause 415 and 406 errors.

5. Query parameter checks. They require parsing and can cause 400 if the
parsing fails. Query parameter checks provide inputs to the handler

6. Header Checks. They also require parsing and can cause 400 if parsing fails.

7. Body check. The request body check can cause 400.
-}
data Delayed e env c where
  Delayed ::
    { capturesD :: env -> DelayedIO e captures
    , methodD :: DelayedIO e ()
    , authD :: DelayedIO e auth
    , acceptD :: DelayedIO e ()
    , contentD :: DelayedIO e contentType
    , paramsD :: DelayedIO e params
    , headersD :: DelayedIO e headers
    , bodyD :: contentType -> DelayedIO e body
    , serverD ::
        captures ->
        params ->
        headers ->
        auth ->
        body ->
        RoutingRequest ->
        RouteResult c
    } ->
    Delayed e env c

instance Functor (Delayed e env) where
  fmap f Delayed {..} =
    Delayed
      { serverD = \c p h a b req -> f <$> serverD c p h a b req
      , ..
      } -- Note [Existential Record Update]

-- | A 'Delayed' without any stored checks.
emptyDelayed :: RouteResult a -> Delayed e env a
emptyDelayed result =
  Delayed (const r) r r r r r r (const r) (\_ _ _ _ _ _ -> result)
  where
    r = return ()

-- | Add a capture to the end of the capture block.
addCapture ::
  Delayed e env (a -> b) ->
  (captured -> DelayedIO e a) ->
  Delayed e (captured, env) b
addCapture Delayed {..} new =
  Delayed
    { capturesD = \(txt, env) -> (,) <$> capturesD env <*> new txt
    , serverD = \(x, v) p h a b req -> ($ v) <$> serverD x p h a b req
    , ..
    } -- Note [Existential Record Update]

-- | Add a parameter check to the end of the params block
addParameterCheck ::
  Delayed e env (a -> b) ->
  DelayedIO e a ->
  Delayed e env b
addParameterCheck Delayed {..} new =
  Delayed
    { paramsD = (,) <$> paramsD <*> new
    , serverD = \c (p, pNew) h a b req -> ($ pNew) <$> serverD c p h a b req
    , ..
    }

-- | Add a parameter check to the end of the params block
addHeaderCheck ::
  Delayed e env (a -> b) ->
  DelayedIO e a ->
  Delayed e env b
addHeaderCheck Delayed {..} new =
  Delayed
    { headersD = (,) <$> headersD <*> new
    , serverD = \c p (h, hNew) a b req -> ($ hNew) <$> serverD c p h a b req
    , ..
    }

-- | Add a method check to the end of the method block.
addMethodCheck ::
  Delayed e env a ->
  DelayedIO e () ->
  Delayed e env a
addMethodCheck Delayed {..} new =
  Delayed
    { methodD = methodD <* new
    , ..
    } -- Note [Existential Record Update]

-- | Add an auth check to the end of the auth block.
addAuthCheck ::
  Delayed e env (a -> b) ->
  DelayedIO e a ->
  Delayed e env b
addAuthCheck Delayed {..} new =
  Delayed
    { authD = (,) <$> authD <*> new
    , serverD = \c p h (y, v) b req -> ($ v) <$> serverD c p h y b req
    , ..
    } -- Note [Existential Record Update]

{- | Add a content type and body checks around parameter checks.

We'll report failed content type check (415), before trying to parse
query parameters (400). Which, in turn, happens before request body parsing.
-}
addBodyCheck ::
  Delayed e env (a -> b) ->
  -- | content type check
  DelayedIO e c ->
  -- | body check
  (c -> DelayedIO e a) ->
  Delayed e env b
addBodyCheck Delayed {..} newContentD newBodyD =
  Delayed
    { contentD = (,) <$> contentD <*> newContentD
    , bodyD = \(content, c) -> (,) <$> bodyD content <*> newBodyD c
    , serverD = \c p h a (z, v) req -> ($ v) <$> serverD c p h a z req
    , ..
    } -- Note [Existential Record Update]

{- | Add an accept header check before handling parameters.
In principle, we'd like
to take a bad body (400) response take precedence over a
failed accept check (406). BUT to allow streaming the body,
we cannot run the body check and then still backtrack.
We therefore do the accept check before the body check,
when we can still backtrack. There are other solutions to
this, but they'd be more complicated (such as delaying the
body check further so that it can still be run in a situation
where we'd otherwise report 406).
-}
addAcceptCheck ::
  Delayed e env a ->
  DelayedIO e () ->
  Delayed e env a
addAcceptCheck Delayed {..} new =
  Delayed
    { acceptD = acceptD *> new
    , ..
    } -- Note [Existential Record Update]

{- | Many combinators extract information that is passed to
the handler without the possibility of failure. In such a
case, 'passToServer' can be used.
-}
passToServer :: Delayed e env (a -> b) -> (RoutingRequest -> a) -> Delayed e env b
passToServer Delayed {..} x =
  Delayed
    { serverD = \c p h a b req -> ($ x req) <$> serverD c p h a b req
    , ..
    } -- Note [Existential Record Update]

{- | Run a delayed server. Performs all scheduled operations
in order, and passes the results from the capture and body
blocks on to the actual handler.

This should only be called once per request; otherwise the guarantees about
effect and HTTP error ordering break down.
-}
runDelayed ::
  Delayed e env a ->
  env ->
  RoutingRequest ->
  JSObject e ->
  FetchContext ->
  ResourceT IO (RouteResult a)
runDelayed Delayed {..} env = runDelayedIO $ do
  HandlerEnv {..} <- ask
  c <- capturesD env
  methodD
  a <- authD
  acceptD
  content <- contentD
  p <- paramsD -- Has to be before body parsing, but after content-type checks
  h <- headersD
  b <- bodyD content
  liftRouteResult (serverD c p h a b request)

{- | Runs a delayed server and the resulting action.
Takes a continuation that lets us send a response.
Also takes a continuation for how to turn the
result of the delayed server into a response.
-}
runAction ::
  Delayed e env (Handler e a) ->
  env ->
  RoutingRequest ->
  JSObject e ->
  FetchContext ->
  (RouteResult RoutingResponse -> IO r) ->
  (a -> RouteResult RoutingResponse) ->
  IO r
runAction action env req bind fenv respond k =
  runResourceT $
    runDelayed action env req bind fenv >>= go
  where
    go (Fail e) = liftIO $ respond $ Fail e
    go (FailFatal e) = liftIO $ respond $ FailFatal e
    go (FastReturn rsp) = liftIO $ respond $ Route $ RawResponse rsp
    go (Route a) = liftIO $ do
      e <- runHandler req bind fenv a
      case e of
        Left err ->
          liftIO . respond . Route . RawResponse
            =<< responseServerReturn err
        Right (x, (getAp .) -> fin) -> liftIO . respond' $ k x
          where
            respond' resl = do
              case resl of
                FastReturn r -> fin r
                Fail err -> fin =<< toWorkerResponse (responseServerError err)
                FailFatal err -> fin =<< toWorkerResponse (responseServerError err)
                Route res -> fin =<< toWorkerResponse res
              respond resl

{- Note [Existential Record Update]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Due to GHC issue <https://ghc.haskell.org/trac/ghc/ticket/2595 2595>, we cannot
do the more succinct thing - just update the records we actually change.
-}
