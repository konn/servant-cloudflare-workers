{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Servant.Cloudflare.Workers.Internal (
  module Servant.Cloudflare.Workers.Internal,
  module Servant.Cloudflare.Workers.Internal.BasicAuth,
  module Servant.Cloudflare.Workers.Internal.Context,
  module Servant.Cloudflare.Workers.Internal.Delayed,
  module Servant.Cloudflare.Workers.Internal.DelayedIO,
  module Servant.Cloudflare.Workers.Internal.ErrorFormatter,
  module Servant.Cloudflare.Workers.Internal.Handler,
  module Servant.Cloudflare.Workers.Internal.Router,
  module Servant.Cloudflare.Workers.Internal.RouteResult,
  module Servant.Cloudflare.Workers.Internal.RoutingApplication,
  module Servant.Cloudflare.Workers.Internal.ServerError,
) where

import Control.Monad (
  guard,
  join,
  when,
 )
import Control.Monad.Trans (
  lift,
  liftIO,
 )
import Control.Monad.Trans.Resource (
  ReleaseKey,
  runResourceT,
 )
import Data.Acquire
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Constraint (Constraint, Dict (..))
import Data.Either (
  partitionEithers,
 )
import Data.Kind (
  Type,
 )
import Data.Maybe (
  fromMaybe,
  isNothing,
  mapMaybe,
  maybeToList,
 )
import Data.String (
  IsString (..),
 )
import Data.Tagged (
  Tagged (..),
  retag,
  untag,
 )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), KnownNat, KnownSymbol, TypeError, symbolVal)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.ReadableStream (ReadableStream, toReadableStream)
import qualified GHC.Wasm.Web.ReadableStream as RS
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext, FetchHandler)
import Network.Cloudflare.Worker.Request (WorkerRequest)
import qualified Network.Cloudflare.Worker.Request as Req
import Network.Cloudflare.Worker.Response (WorkerResponse, WorkerResponseBody (..))
import qualified Network.HTTP.Media as NHM
import Network.HTTP.Types hiding (
  Header,
  ResponseHeaders,
 )
import qualified Network.HTTP.Types as H
import Servant.API (
  Accept (..),
  BasicAuth,
  Capture',
  CaptureAll,
  DeepQuery,
  Description,
  EmptyAPI,
  Fragment,
  Header',
  If,
  IsSecure (..),
  NamedRoutes,
  NoContentVerb,
  QueryFlag,
  QueryParam',
  QueryParams,
  QueryString,
  Raw,
  RawM,
  ReflectMethod (reflectMethod),
  ReqBody',
  SBool (..),
  SBoolI (..),
  Stream,
  StreamBody',
  Summary,
  Verb,
  WithNamedContext,
  WithResource,
  (:<|>) (..),
  (:>),
 )
import Servant.API.ContentTypes (
  AcceptHeader (..),
  AllCTRender (..),
  AllCTUnrender (..),
  AllMime,
  NoContent,
  canHandleAcceptH,
 )
import Servant.API.Generic (GServantProduct, GenericMode (..), ToServant, ToServantApi, fromServant, toServant)
import Servant.API.Modifiers (
  FoldLenient,
  FoldRequired,
  RequestArgument,
  unfoldRequestArgument,
 )
import Servant.API.QueryString (FromDeepQuery (..))
import Servant.API.ResponseHeaders (
  GetHeaders,
  Headers,
  getHeaders,
  getResponse,
 )
import Servant.API.Status (
  statusFromNat,
 )
import Servant.API.TypeErrors
import Servant.API.TypeLevel (AtMostOneFragment, FragmentUnique)
import Servant.Cloudflare.Workers.Internal.BasicAuth
import Servant.Cloudflare.Workers.Internal.Context
import Servant.Cloudflare.Workers.Internal.Delayed
import Servant.Cloudflare.Workers.Internal.DelayedIO
import Servant.Cloudflare.Workers.Internal.ErrorFormatter
import Servant.Cloudflare.Workers.Internal.Handler
import Servant.Cloudflare.Workers.Internal.Response
import Servant.Cloudflare.Workers.Internal.RouteResult
import Servant.Cloudflare.Workers.Internal.Router
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import Servant.Cloudflare.Workers.Internal.ServerError
import Servant.Cloudflare.Workers.ReadableStream
import qualified Streaming.ByteString as Q
import System.IO.Unsafe (unsafePerformIO)
import Web.HttpApiData (
  FromHttpApiData,
  parseHeader,
  parseQueryParam,
  parseUrlPiece,
  parseUrlPieces,
 )

class HasWorker (e :: Prototype) api context where
  -- | The type of a server for this API, given a monad to run effects in.
  --
  -- Note that the result kind is @*@, so it is /not/ a monad transformer, unlike
  -- what the @T@ in the name might suggest.
  type WorkerT e api (m :: Type -> Type) :: Type

  route ::
    Proxy e ->
    Proxy api ->
    Context context ->
    Delayed env (Worker e api) ->
    Router e env

  hoistWorkerWithContext ::
    Proxy e ->
    Proxy api ->
    Proxy context ->
    (forall x. m x -> n x) ->
    WorkerT e api m ->
    WorkerT e api n

type Worker e api = WorkerT e api (Handler e)

-- * Instances

{- | A server for @a ':<|>' b@ first tries to match the request against the route
  represented by @a@ and if it fails tries @b@. You must provide a request
  handler for each route.

> type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
>         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
>
> server :: Worker MyApi
> server = listAllBooks :<|> postBook
>   where listAllBooks = ...
>         postBook book = ...
-}
instance (HasWorker e a context, HasWorker e b context) => HasWorker e (a :<|> b) context where
  type WorkerT e (a :<|> b) m = WorkerT e a m :<|> WorkerT e b m

  route e Proxy context server =
    choice
      (route e pa context ((\(a :<|> _) -> a) <$> server))
      (route e pb context ((\(_ :<|> b) -> b) <$> server))
    where
      pa = Proxy :: Proxy a
      pb = Proxy :: Proxy b

  -- \| This is better than 'enter', as it's tailor made for 'HasWorker'.
  hoistWorkerWithContext e _ pc nt (a :<|> b) =
    hoistWorkerWithContext e (Proxy :: Proxy a) pc nt a
      :<|> hoistWorkerWithContext e (Proxy :: Proxy b) pc nt b

{- | If you use 'Capture' in one of the endpoints for your API,
this automatically requires your server-side handler to be a function
that takes an argument of the type specified by the 'Capture'.
This lets servant worry about getting it from the URL and turning
it into a value of the type you specify.

You can control how it'll be converted from 'Text' to your type
by simply providing an instance of 'FromHttpApiData' for your type.

Example:

> type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
>
> server :: Worker MyApi
> server = getBook
>   where getBook :: Text -> Handler Book
>         getBook isbn = ...
-}
instance
  ( KnownSymbol capture
  , FromHttpApiData a
  , Typeable a
  , HasWorker e api context
  , SBoolI (FoldLenient mods)
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasWorker e (Capture' mods capture a :> api) context
  where
  type
    WorkerT e (Capture' mods capture a :> api) m =
      If (FoldLenient mods) (Either String a) a -> WorkerT e api m

  hoistWorkerWithContext e _ pc nt s = hoistWorkerWithContext e (Proxy :: Proxy api) pc nt . s

  route pe Proxy context d =
    CaptureRouter [hint] $
      route
        pe
        (Proxy :: Proxy api)
        context
        ( addCapture d $ \txt -> withRequest $ \request ->
            case ( sbool :: SBool (FoldLenient mods)
                 , parseUrlPiece txt :: Either T.Text a
                 ) of
              (SFalse, Left e) -> delayedFail $ formatError rep request $ T.unpack e
              (SFalse, Right v) -> return v
              (STrue, piece) -> return $ either (Left . T.unpack) Right piece
        )
    where
      rep = typeRep (Proxy :: Proxy Capture')
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)
      hint = CaptureHint (T.pack $ symbolVal $ Proxy @capture) (typeRep (Proxy :: Proxy a))

{- | If you use 'CaptureAll' in one of the endpoints for your API,
this automatically requires your server-side handler to be a
function that takes an argument of a list of the type specified by
the 'CaptureAll'. This lets servant worry about getting values from
the URL and turning them into values of the type you specify.

You can control how they'll be converted from 'Text' to your type
by simply providing an instance of 'FromHttpApiData' for your type.

Example:

> type MyApi = "src" :> CaptureAll "segments" Text :> Get '[JSON] SourceFile
>
> server :: Worker MyApi
> server = getSourceFile
>   where getSourceFile :: [Text] -> Handler Book
>         getSourceFile pathSegments = ...
-}
instance
  ( KnownSymbol capture
  , FromHttpApiData a
  , Typeable a
  , HasWorker e api context
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasWorker e (CaptureAll capture a :> api) context
  where
  type
    WorkerT e (CaptureAll capture a :> api) m =
      [a] -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context d =
    CaptureAllRouter [hint] $
      route
        pe
        (Proxy :: Proxy api)
        context
        ( addCapture d $ \txts -> withRequest $ \request ->
            case parseUrlPieces txts of
              Left e -> delayedFail $ formatError rep request $ T.unpack e
              Right v -> return v
        )
    where
      rep = typeRep (Proxy :: Proxy CaptureAll)
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)
      hint = CaptureHint (T.pack $ symbolVal $ Proxy @capture) (typeRep (Proxy :: Proxy [a]))

{- | If you use 'WithResource' in one of the endpoints for your API Servant
will provide the handler for this endpoint an argument of the specified type.
The lifespan of this resource will be automatically managed by Servant. This
resource will be created before the handler starts and it will be destoyed
after it ends. A new resource is created for each request to the endpoint.
-}

-- The creation and destruction are done using a 'Data.Acquire.Acquire'
-- provided via server 'Context'.
--
-- Example
--
-- > type MyApi = WithResource Handle :> "writeToFile" :> Post '[JSON] NoContent
-- >
-- > server :: Worker MyApi
-- > server = writeToFile
-- >   where writeToFile :: (ReleaseKey, Handle) -> Handler NoContent
-- >         writeToFile (_, h) = hPutStrLn h "message"
--
-- In addition to the resource, the handler will also receive a 'ReleaseKey'
-- which can be used to deallocate the resource before the end of the request
-- if desired.

instance
  (HasWorker e api ctx, HasContextEntry ctx (Acquire a)) =>
  HasWorker e (WithResource a :> api) ctx
  where
  type WorkerT e (WithResource a :> api) m = (ReleaseKey, a) -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy @api) pc nt . s

  route pe Proxy context d = route pe (Proxy @api) context (d `addParameterCheck` allocateResource)
    where
      allocateResource :: DelayedIO (ReleaseKey, a)
      allocateResource = DelayedIO $ lift $ allocateAcquire (getContextEntry context)

fromJSBS :: JSByteString -> B.ByteString
{-# NOINLINE fromJSBS #-}
fromJSBS = unsafePerformIO . toHaskellByteString

requestMethod :: RoutingRequest -> BC8.ByteString
requestMethod = fromJSBS . Req.getMethod . rawRequest

allowedMethodHead :: Method -> RoutingRequest -> Bool
allowedMethodHead method request = method == methodGet && requestMethod request == methodHead

allowedMethod :: Method -> RoutingRequest -> Bool
allowedMethod method request = allowedMethodHead method request || requestMethod request == method

methodCheck :: Method -> RoutingRequest -> DelayedIO ()
methodCheck method request
  | allowedMethod method request = return ()
  | otherwise = delayedFail err405

-- This has switched between using 'Fail' and 'FailFatal' a number of
-- times. If the 'acceptCheck' is run after the body check (which would
-- be morally right), then we have to set this to 'FailFatal', because
-- the body check is not reversible, and therefore backtracking after the
-- body check is no longer an option. However, we now run the accept
-- check before the body check and can therefore afford to make it
-- recoverable.
acceptCheck :: (AllMime list) => Proxy list -> AcceptHeader -> DelayedIO ()
acceptCheck proxy accH
  | canHandleAcceptH proxy accH = return ()
  | otherwise = delayedFail err406

methodRouter ::
  forall ctypes a e b env.
  (AllCTRender ctypes a) =>
  (b -> ([(HeaderName, B.ByteString)], a)) ->
  Method ->
  Proxy ctypes ->
  Status ->
  Delayed env (Handler e b) ->
  Router e env
methodRouter splitHeaders method proxy status action = leafRouter route'
  where
    route' :: env -> RoutingApplication e
    route' env request obj fctx respond =
      let accH = getAcceptHeader request.rawRequest
       in runAction
            ( action
                `addMethodCheck` methodCheck method request
                `addAcceptCheck` acceptCheck proxy accH
            )
            env
            request.rawRequest
            obj
            fctx
            respond
            $ \output -> do
              let (headers, b) = splitHeaders output
              case handleAcceptH proxy accH b of
                Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
                Just (contentT, body) ->
                  let bdy = if allowedMethodHead method request then "" else body
                   in Route $ responseLBS status ((hContentType, BSL.toStrict contentT) : headers) bdy

responseLBS ::
  Status ->
  [(HeaderName, B.StrictByteString)] ->
  BSL.ByteString ->
  PartialResponse
responseLBS status headers bdy =
  PartialResponse
    { status = status
    , headers
    , encodeBody = Nothing
    , cloudflare = Nothing
    , body = do
        guard $ not $ BSL.null bdy
        Just $ WorkerResponseLBS bdy
    }

noContentRouter ::
  Method ->
  Status ->
  Delayed env (Handler e b) ->
  Router e env
noContentRouter method status action = leafRouter route'
  where
    route' env request b ctx respond =
      runAction
        (action `addMethodCheck` methodCheck method request)
        env
        request.rawRequest
        b
        ctx
        respond
        $ \_output ->
          Route $ responseLBS status [] ""

instance
  {-# OVERLAPPABLE #-}
  ( AllCTRender ctypes a
  , ReflectMethod method
  , KnownNat status
  ) =>
  HasWorker e (Verb method status ctypes a) context
  where
  type WorkerT e (Verb method status ctypes a) m = m a
  hoistWorkerWithContext _ _ _ nt s = nt s

  route Proxy _ _ = methodRouter ([],) method (Proxy :: Proxy ctypes) status
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)

instance
  {-# OVERLAPPING #-}
  ( AllCTRender ctypes a
  , ReflectMethod method
  , KnownNat status
  , GetHeaders (Headers h a)
  ) =>
  HasWorker e (Verb method status ctypes (Headers h a)) context
  where
  type WorkerT e (Verb method status ctypes (Headers h a)) m = m (Headers h a)
  hoistWorkerWithContext _ _ _ nt s = nt s

  route Proxy _ _ = methodRouter (\x -> (getHeaders x, getResponse x)) method (Proxy :: Proxy ctypes) status
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)

instance
  (ReflectMethod method) =>
  HasWorker e (NoContentVerb method) context
  where
  type WorkerT e (NoContentVerb method) m = m NoContent
  hoistWorkerWithContext _ _ _ nt s = nt s

  route Proxy _ _ = noContentRouter method status204
    where
      method = reflectMethod (Proxy :: Proxy method)

instance
  {-# OVERLAPPABLE #-}
  ( Accept ctype
  , ReflectMethod method
  , KnownNat status
  , ToReadableStream a
  ) =>
  HasWorker e (Stream method status framing ctype a) context
  where
  type WorkerT e (Stream method status framing ctype a) m = m a
  hoistWorkerWithContext _ _ _ nt s = nt s

  route Proxy _ _ = streamRouter ([],) method status (Proxy :: Proxy framing) (Proxy :: Proxy ctype)
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)

instance
  {-# OVERLAPPING #-}
  ( Accept ctype
  , ReflectMethod method
  , KnownNat status
  , ToReadableStream a
  , GetHeaders (Headers h a)
  ) =>
  HasWorker e (Stream method status framing ctype (Headers h a)) context
  where
  type WorkerT e (Stream method status framing ctype (Headers h a)) m = m (Headers h a)
  hoistWorkerWithContext _ _ _ nt s = nt s

  route Proxy _ _ = streamRouter (\x -> (getHeaders x, getResponse x)) method status (Proxy :: Proxy framing) (Proxy :: Proxy ctype)
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)

streamRouter ::
  forall e ctype a c env framing.
  (Accept ctype, ToReadableStream a) =>
  (c -> ([(HeaderName, B.ByteString)], a)) ->
  Method ->
  Status ->
  Proxy framing ->
  Proxy ctype ->
  Delayed env (Handler e c) ->
  Router e env
streamRouter splitHeaders method status _ ctypeproxy action = leafRouter $ \env request b fctx respond ->
  let AcceptHeader accH = getAcceptHeader request.rawRequest
      cmediatype = NHM.matchAccept [contentType ctypeproxy] accH
      accCheck = when (isNothing cmediatype) $ delayedFail err406
      contentHeader = (hContentType, NHM.renderHeader . maybeToList $ cmediatype)
   in runAction
        ( action
            `addMethodCheck` methodCheck method request
            `addAcceptCheck` accCheck
        )
        env
        request.rawRequest
        b
        fctx
        respond
        $ \output ->
          let (headers, fa) = splitHeaders output
           in Route $ responseStream status (contentHeader : headers) (toReadableStream' fa)

responseStream :: Status -> [(HeaderName, BC8.ByteString)] -> RS.ReadableStream -> PartialResponse
responseStream stt hdrs str =
  PartialResponse
    { status = stt
    , headers = hdrs
    , encodeBody = Nothing
    , cloudflare = Nothing
    , body = Just $ WorkerResponseStream str
    }

{- | If you use 'Header' in one of the endpoints for your API,
this automatically requires your server-side handler to be a function
that takes an argument of the type specified by 'Header'.
This lets servant worry about extracting it from the request and turning
it into a value of the type you specify.

All it asks is for a 'FromHttpApiData' instance.

Example:

> newtype Referer = Referer Text
>   deriving (Eq, Show, FromHttpApiData)
>
>            -- GET /view-my-referer
> type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
>
> server :: Worker MyApi
> server = viewReferer
>   where viewReferer :: Referer -> Handler referer
>         viewReferer referer = return referer
-}
instance
  ( KnownSymbol sym
  , FromHttpApiData a
  , HasWorker e api context
  , SBoolI (FoldRequired mods)
  , SBoolI (FoldLenient mods)
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasWorker e (Header' mods sym a :> api) context
  where
  ------
  type
    WorkerT e (Header' mods sym a :> api) m =
      RequestArgument mods a -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context $
      subserver `addHeaderCheck` withRequest headerCheck
    where
      rep = typeRep (Proxy :: Proxy Header')
      formatError = headerParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      headerName :: (IsString n) => n
      headerName = fromString $ symbolVal (Proxy :: Proxy sym)

      headerCheck :: WorkerRequest -> DelayedIO (RequestArgument mods a)
      headerCheck req =
        unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
        where
          mev :: Maybe (Either T.Text a)
          mev = fmap parseHeader $ lookup headerName (requestHeaders req)

          errReq =
            delayedFailFatal $
              formatError rep req $
                "Header " <> headerName <> " is required"

          errSt e =
            delayedFailFatal $
              formatError rep req $
                T.unpack $
                  "Error parsing header "
                    <> headerName
                    <> " failed: "
                    <> e

{- | If you use @'QueryParam' "author" Text@ in one of the endpoints for your API,
this automatically requires your server-side handler to be a function
that takes an argument of type @'Maybe' 'Text'@.

This lets servant worry about looking it up in the query string
and turning it into a value of the type you specify, enclosed
in 'Maybe', because it may not be there and servant would then
hand you 'Nothing'.

You can control how it'll be converted from 'Text' to your type
by simply providing an instance of 'FromHttpApiData' for your type.

Example:

> type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
>
> server :: Worker MyApi
> server = getBooksBy
>   where getBooksBy :: Maybe Text -> Handler [Book]
>         getBooksBy Nothing       = ...return all books...
>         getBooksBy (Just author) = ...return books by the given author...
-}
instance
  ( KnownSymbol sym
  , FromHttpApiData a
  , HasWorker e api context
  , SBoolI (FoldRequired mods)
  , SBoolI (FoldLenient mods)
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasWorker e (QueryParam' mods sym a :> api) context
  where
  ------
  type
    WorkerT e (QueryParam' mods sym a :> api) m =
      RequestArgument mods a -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    let querytext = queryToQueryText . queryString
        paramname = T.pack $ symbolVal (Proxy :: Proxy sym)

        rep = typeRep (Proxy :: Proxy QueryParam')
        formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

        parseParam :: WorkerRequest -> DelayedIO (RequestArgument mods a)
        parseParam req =
          unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
          where
            mev :: Maybe (Either T.Text a)
            mev = fmap parseQueryParam $ join $ lookup paramname $ querytext req

            errReq =
              delayedFailFatal $
                formatError rep req $
                  T.unpack $
                    "Query parameter " <> paramname <> " is required"

            errSt e =
              delayedFailFatal $
                formatError rep req $
                  T.unpack $
                    "Error parsing query parameter "
                      <> paramname
                      <> " failed: "
                      <> e

        delayed = addParameterCheck subserver . withRequest $ \req ->
          parseParam req
     in route pe (Proxy :: Proxy api) context delayed

queryString :: WorkerRequest -> Query
queryString = snd . decodePath . TE.encodeUtf8 . Req.getUrl

{- | If you use @'QueryParams' "authors" Text@ in one of the endpoints for your API,
this automatically requires your server-side handler to be a function
that takes an argument of type @['Text']@.

This lets servant worry about looking up 0 or more values in the query string
associated to @authors@ and turning each of them into a value of
the type you specify.

You can control how the individual values are converted from 'Text' to your type
by simply providing an instance of 'FromHttpApiData' for your type.

Example:

> type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
>
> server :: Worker MyApi
> server = getBooksBy
>   where getBooksBy :: [Text] -> Handler [Book]
>         getBooksBy authors = ...return all books by these authors...
-}
instance
  ( KnownSymbol sym
  , FromHttpApiData a
  , HasWorker e api context
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasWorker e (QueryParams sym a :> api) context
  where
  type
    WorkerT e (QueryParams sym a :> api) m =
      [a] -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context $
      subserver `addParameterCheck` withRequest paramsCheck
    where
      rep = typeRep (Proxy :: Proxy QueryParams)
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      paramname = T.pack $ symbolVal (Proxy :: Proxy sym)
      paramsCheck req =
        case partitionEithers $ fmap parseQueryParam params of
          ([], parsed) -> return parsed
          (errs, _) ->
            delayedFailFatal $
              formatError rep req $
                T.unpack $
                  "Error parsing query parameter(s) "
                    <> paramname
                    <> " failed: "
                    <> T.intercalate ", " errs
        where
          params :: [T.Text]
          params =
            mapMaybe snd
              . filter (looksLikeParam . fst)
              . queryToQueryText
              . snd
              . decodePath
              . extractPath
              . TE.encodeUtf8
              $ Req.getUrl req

          looksLikeParam name = name == paramname || name == (paramname <> "[]")

{- | If you use @'QueryFlag' "published"@ in one of the endpoints for your API,
this automatically requires your server-side handler to be a function
that takes an argument of type 'Bool'.

Example:

> type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
>
> server :: Worker MyApi
> server = getBooks
>   where getBooks :: Bool -> Handler [Book]
>         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
-}
instance
  (KnownSymbol sym, HasWorker e api context) =>
  HasWorker e (QueryFlag sym :> api) context
  where
  type
    WorkerT e (QueryFlag sym :> api) m =
      Bool -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    let querytext = queryToQueryText . queryString
        param r = case lookup paramname (querytext r) of
          Just Nothing -> True -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing -> False -- param not in the query string
     in route pe (Proxy :: Proxy api) context (passToServer subserver param)
    where
      paramname = T.pack $ symbolVal (Proxy :: Proxy sym)
      examine v
        | v == "true" || v == "1" || v == "" = True
        | otherwise = False

{- | If you use @'QueryString'@ in one of the endpoints for your API,
this automatically requires your server-side handler to be a function
that takes an argument of type @Query@ (@[('ByteString', 'Maybe' 'ByteString')]@).

This lets you extract the whole query string. This is useful when the query string
can contain parameters with dynamic names, that you can't access with @'QueryParam'@.

Example:

> type MyApi = "books" :> QueryString :> Get '[JSON] [Book]
>
> server :: Worker MyApi
> server = getBooksBy
>   where getBooksBy :: Query -> Handler [Book]
>         getBooksBy filters = ...filter books based on the dynamic filters provided...
-}
instance
  ( HasWorker e api context
  ) =>
  HasWorker e (QueryString :> api) context
  where
  ------
  type
    WorkerT e (QueryString :> api) m =
      Query -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context (passToServer subserver queryString)

{- | If you use @'DeepQuery' "symbol" a@ in one of the endpoints for your API,
this automatically requires your server-side handler to be a function
that takes an argument of type @a@.

This lets you extract an object from multiple parameters in the query string,
with its fields enclosed in brackets: `/books?filter[author][name]=value`. When
all the fields are known in advance, it can be done with @'QueryParam'@ (it can
still be tedious if you the object has many fields). When some fields are dynamic,
it cannot be done with @'QueryParam'.

The way the object is constructed from the extracted fields can be controlled by
providing an instance on @'FromDeepQuery'@

Example:

> type MyApi = "books" :> DeepQuery "filter" BookQuery :> Get '[JSON] [Book]
>
> server :: Worker MyApi
> server = getBooksBy
>   where getBooksBy :: BookQuery -> Handler [Book]
>         getBooksBy query = ...filter books based on the dynamic filters provided...
-}
instance
  ( KnownSymbol sym
  , FromDeepQuery a
  , HasWorker e api context
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasWorker e (DeepQuery sym a :> api) context
  where
  ------
  type
    WorkerT e (DeepQuery sym a :> api) m =
      a -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context $
      subserver `addParameterCheck` withRequest paramsCheck
    where
      rep = typeRep (Proxy :: Proxy DeepQuery)
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      paramname = T.pack $ symbolVal (Proxy :: Proxy sym)
      paramsCheck req =
        let relevantParams :: [(T.Text, Maybe T.Text)]
            relevantParams =
              mapMaybe isRelevantParam
                . queryToQueryText
                . queryString
                $ req
            isRelevantParam (name, value) =
              (,value)
                <$> case T.stripPrefix paramname name of
                  Just "" -> Just ""
                  Just x | "[" `T.isPrefixOf` x -> Just x
                  _ -> Nothing
         in case fromDeepQuery =<< traverse parseDeepParam relevantParams of
              Left e ->
                delayedFailFatal $
                  formatError rep req $
                    T.unpack $
                      "Error parsing deep query parameter(s) "
                        <> paramname
                        <> T.pack " failed: "
                        <> T.pack e
              Right parsed -> return parsed

parseDeepParam :: (T.Text, Maybe T.Text) -> Either String ([T.Text], Maybe T.Text)
parseDeepParam (paramname, value) =
  let parseParam "" = return []
      parseParam n = reverse <$> go [] n
      go parsed remaining = case T.take 1 remaining of
        "[" -> case T.breakOn "]" remaining of
          (_, "") -> Left $ "Error parsing deep param, missing closing ']': " <> T.unpack remaining
          (name, "]") -> return $ T.drop 1 name : parsed
          (name, remaining') -> case T.take 2 remaining' of
            "][" -> go (T.drop 1 name : parsed) (T.drop 1 remaining')
            _ -> Left $ "Error parsing deep param, incorrect brackets: " <> T.unpack remaining
        _ -> Left $ "Error parsing deep param, missing opening '[': " <> T.unpack remaining
   in (,value) <$> parseParam paramname

{- | Just pass the request to the underlying application and serve its response.

Example:

> type MyApi = "images" :> Raw
>
> server :: Worker MyApi
> server = serveDirectory "/var/www/images"
-}
instance HasWorker e Raw context where
  type WorkerT e Raw m = Tagged m (FetchHandler e)

  hoistWorkerWithContext _ _ _ _ = retag

  route _ Proxy _ rawApplication = RawRouter $ \env request wenv fctx respond -> runResourceT $ do
    -- note: a Raw application doesn't register any cleanup
    -- but for the sake of consistency, we nonetheless run
    -- the cleanup once its done
    r <- runDelayed rawApplication env request.rawRequest
    liftIO $ go r request wenv fctx respond
    where
      go r request wenv fctx respond = case r of
        Route app -> untag app request.rawRequest wenv fctx
        Fail a -> respond $ Fail a
        FailFatal e -> respond $ FailFatal e

{- | Just pass the request to the underlying application and serve its response.

Example:

> type MyApi = "images" :> Raw
>
> server :: Worker MyApi
> server = serveDirectory "/var/www/images"
-}
instance HasWorker e RawM context where
  type
    WorkerT e RawM m =
      RoutingRequest ->
      JSObject e ->
      FetchContext ->
      (RouteResult PartialResponse -> IO WorkerResponse) ->
      m WorkerResponse

  route _ _ _ handleDelayed = RawRouter $ \env request b fctx respond -> runResourceT $ do
    routeResult <- runDelayed handleDelayed env request.rawRequest
    let respond' = liftIO . respond
    liftIO $ case routeResult of
      Route handler ->
        runHandler b fctx (handler request b fctx respond)
          >>= \case
            Left e -> respond' $ FailFatal e
            Right a -> pure a
      {- runHandler (handler request.rawRequest fctx)
        -}
      Fail e -> respond' $ Fail e
      FailFatal e -> respond' $ FailFatal e

  hoistWorkerWithContext _ _ _ f srvM = \req b fctx respond -> f (srvM req b fctx respond)

{- | If you use 'ReqBody' in one of the endpoints for your API,
this automatically requires your server-side handler to be a function
that takes an argument of the type specified by 'ReqBody'.
The @Content-Type@ header is inspected, and the list provided is used to
attempt deserialization. If the request does not have a @Content-Type@
header, it is treated as @application/octet-stream@ (as specified in
[RFC 7231 section 3.1.1.5](http://tools.ietf.org/html/rfc7231#section-3.1.1.5)).
This lets servant worry about extracting it from the request and turning
it into a value of the type you specify.


All it asks is for a 'FromJSON' instance.

Example:

> type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
>
> server :: Worker MyApi
> server = postBook
>   where postBook :: Book -> Handler Book
>         postBook book = ...insert into your db...
-}
instance
  ( AllCTUnrender list a
  , HasWorker e api context
  , SBoolI (FoldLenient mods)
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasWorker e (ReqBody' mods list a :> api) context
  where
  type
    WorkerT e (ReqBody' mods list a :> api) m =
      If (FoldLenient mods) (Either String a) a -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s =
    hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context $
      addBodyCheck subserver ctCheck bodyCheck
    where
      rep = typeRep (Proxy :: Proxy ReqBody')
      formatError = bodyParserErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      -- Content-Type check, we only lookup we can try to parse the request body
      ctCheck = withRequest $ \request -> do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH =
              fromMaybe "application/octet-stream" $
                lookup hContentType $
                  requestHeaders request
        case canHandleCTypeH (Proxy :: Proxy list) (BSL.fromStrict contentTypeH) :: Maybe (BSL.ByteString -> Either String a) of
          Nothing -> delayedFail err415
          Just f -> return f

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck f = withRequest $ \request -> do
        mrqbody <- f <$> liftIO (lazyRequestBody request)
        case sbool :: SBool (FoldLenient mods) of
          STrue -> return mrqbody
          SFalse -> case mrqbody of
            Left e -> delayedFailFatal $ formatError rep request e
            Right v -> return v

lazyRequestBody :: WorkerRequest -> IO BSL.ByteString
lazyRequestBody =
  nullable (pure mempty) (Q.toLazy_ . RS.fromReadableStream) . Req.getBody

instance
  ( FromReadableStream a
  , HasWorker e api context
  ) =>
  HasWorker e (StreamBody' mods framing ctype a :> api) context
  where
  type WorkerT e (StreamBody' mods framing ctype a :> api) m = a -> WorkerT e api m

  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context $
      addBodyCheck subserver ctCheck bodyCheck
    where
      ctCheck :: DelayedIO (ReadableStream -> IO a)
      -- TODO: do content-type check
      ctCheck = return fromReadableStreamIO

      bodyCheck :: (ReadableStream -> IO a) -> DelayedIO a
      bodyCheck fromRS = withRequest $ \req -> do
        liftIO $ fromRS =<< reqBodyToStream req

reqBodyToStream :: WorkerRequest -> IO ReadableStream
reqBodyToStream =
  nullable (toReadableStream mempty) pure . Req.getBody

{- | Make sure the incoming request starts with @"/path"@, strip it and
pass the rest of the request path to @api@.
-}
instance (KnownSymbol path, HasWorker e api context) => HasWorker e (path :> api) context where
  type WorkerT e (path :> api) m = WorkerT e api m

  route pe Proxy context subserver =
    pathRouter
      (T.pack (symbolVal proxyPath))
      (route pe (Proxy :: Proxy api) context subserver)
    where
      proxyPath = Proxy :: Proxy path
  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt s

-- TODO: lookup from Headers and/or cf params
{-
instance (HasWorker api context) => HasWorker (RemoteHost :> api) context where
  type WorkerT (RemoteHost :> api) m = SockAddr -> WorkerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver remoteHost)
  hoistWorkerWithContext _ pc nt s = hoistWorkerWithContext (Proxy :: Proxy api) pc nt . s -}

instance (HasWorker e api context) => HasWorker e (IsSecure :> api) context where
  type WorkerT e (IsSecure :> api) m = IsSecure -> WorkerT e api m

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context (passToServer subserver secure)
    where
      secure req = if isSecure req then Secure else NotSecure

  hoistWorkerWithContext pe _ pc nt s =
    hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

isSecure :: WorkerRequest -> Bool
isSecure = not . T.null . toText . unsafePerformIO . getDictField "tlsVersion" . Req.getCloudflare

instance (HasWorker e api context) => HasWorker e (HttpVersion :> api) context where
  type WorkerT e (HttpVersion :> api) m = HttpVersion -> WorkerT e api m

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context (passToServer subserver httpVersion)
  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

httpVersion :: WorkerRequest -> HttpVersion
httpVersion = error "Not implemented"

-- | Ignore @'Summary'@ in server handlers.
instance (HasWorker e api ctx) => HasWorker e (Summary desc :> api) ctx where
  type WorkerT e (Summary desc :> api) m = WorkerT e api m

  route pe _ = route pe (Proxy :: Proxy api)
  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt s

-- | Ignore @'Description'@ in server handlers.
instance (HasWorker e api ctx) => HasWorker e (Description desc :> api) ctx where
  type WorkerT e (Description desc :> api) m = WorkerT e api m

  route pe _ = route pe (Proxy :: Proxy api)
  hoistWorkerWithContext pe _ pc nt s = hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt s

-- | Singleton type representing a server that serves an empty API.
data EmptyServer = EmptyServer deriving (Typeable, Eq, Show, Bounded, Enum)

-- | Worker for `EmptyAPI`
emptyServer :: WorkerT e EmptyAPI m
emptyServer = Tagged EmptyServer

{- | The server for an `EmptyAPI` is `emptyServer`.

> type MyApi = "nothing" :> EmptyApi
>
> server :: Worker MyApi
> server = emptyServer
-}
instance HasWorker e EmptyAPI context where
  type WorkerT e EmptyAPI m = Tagged m EmptyServer

  route _ Proxy _ _ = StaticRouter mempty mempty

  hoistWorkerWithContext _ _ _ _ = retag

-- | Ignore @'EmptyAPI'@ as part of route in server handlers.
instance (HasWorker e api context) => HasWorker e (EmptyAPI :> api) context where
  type WorkerT e (EmptyAPI :> api) m = WorkerT e api m

  route pe _ = route pe (Proxy :: Proxy api)
  hoistWorkerWithContext pe _ = hoistWorkerWithContext pe (Proxy :: Proxy api)

-- | Basic Authentication
instance
  ( KnownSymbol realm
  , HasWorker e api context
  , HasContextEntry context (BasicAuthCheck usr)
  ) =>
  HasWorker e (BasicAuth realm usr :> api) context
  where
  type WorkerT e (BasicAuth realm usr :> api) m = usr -> WorkerT e api m

  route pe Proxy context subserver =
    route pe (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
      realm = BC8.pack $ symbolVal (Proxy :: Proxy realm)
      basicAuthContext = getContextEntry context
      authCheck = withRequest $ \req -> runBasicAuth req realm basicAuthContext

  hoistWorkerWithContext pe _ pc nt s =
    hoistWorkerWithContext pe (Proxy :: Proxy api) pc nt . s

-- * helpers

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*"

requestHeaders :: WorkerRequest -> [H.Header]
requestHeaders = map (Bi.first CI.mk) . Req.getHeaders

getAcceptHeader :: WorkerRequest -> AcceptHeader
getAcceptHeader = AcceptHeader . fromMaybe ct_wildcard . lookup hAccept . requestHeaders

-- * General Authentication

-- * contexts

instance
  (HasContextEntry context (NamedContext name subContext), HasWorker e subApi subContext) =>
  HasWorker e (WithNamedContext name subContext subApi) context
  where
  type
    WorkerT e (WithNamedContext name subContext subApi) m =
      WorkerT e subApi m

  route pe Proxy context delayed =
    route pe subProxy subContext delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      subContext :: Context subContext
      subContext = descendIntoNamedContext (Proxy :: Proxy name) context

  hoistWorkerWithContext pe _ _ nt s = hoistWorkerWithContext pe (Proxy :: Proxy subApi) (Proxy :: Proxy subContext) nt s

-------------------------------------------------------------------------------
-- Custom type errors
-------------------------------------------------------------------------------

-- Erroring instance for 'HasWorker' when a combinator is not fully applied
instance
  ( TypeError
      ( PartialApplication
          @(Prototype -> Type -> [Type] -> Constraint)
          HasWorker
          arr
      )
  ) =>
  HasWorker e ((arr :: a -> b) :> sub) context
  where
  type WorkerT e (arr :> sub) _ = TypeError (PartialApplication (HasWorker :: Prototype -> Type -> [Type] -> Constraint) arr)
  route = error "unreachable"
  hoistWorkerWithContext _ _ _ _ = error "unreachable"

{- | This instance prevents from accidentally using '->' instead of ':>'

>>> serve (Proxy :: Proxy (Capture "foo" Int -> Get '[JSON] Int)) (error "...")
...
...No instance HasWorker (a -> b).
...Maybe you have used '->' instead of ':>' between
...Capture' '[] "foo" Int
...and
...Verb 'GET 200 '[JSON] Int
...

>>> undefined :: Worker (Capture "foo" Int -> Get '[JSON] Int)
...
...No instance HasWorker (a -> b).
...Maybe you have used '->' instead of ':>' between
...Capture' '[] "foo" Int
...and
...Verb 'GET 200 '[JSON] Int
...
-}
instance (TypeError (HasWorkerArrowTypeError a b)) => HasWorker e (a -> b) context where
  type WorkerT e (a -> b) m = TypeError (HasWorkerArrowTypeError a b)
  route _ _ _ = error "servant-server panic: impossible happened in HasWorker (a -> b)"
  hoistWorkerWithContext _ _ _ _ = id

type HasWorkerArrowTypeError a b =
  'Text "No instance HasWorker (a -> b)."
    ':$$: 'Text "Maybe you have used '->' instead of ':>' between "
    ':$$: 'ShowType a
    ':$$: 'Text "and"
    ':$$: 'ShowType b

-- Erroring instances for 'HasWorker' for unknown API combinators

-- XXX: This omits the @context@ parameter, e.g.:
--
-- "There is no instance for HasWorker (Bool :> …)". Do we care ?
instance
  {-# OVERLAPPABLE #-}
  ( TypeError
      ( NoInstanceForSub
          @(Prototype -> Type -> [Type] -> Constraint)
          HasWorker
          ty
      )
  ) =>
  HasWorker e (ty :> sub) context

instance {-# OVERLAPPABLE #-} (TypeError (NoInstanceFor (HasWorker api context))) => HasWorker e api context

{- | Ignore @'Fragment'@ in server handlers.
See <https://ietf.org/rfc/rfc2616.html#section-15.1.3> for more details.

Example:

> type MyApi = "books" :> Fragment Text :> Get '[JSON] [Book]
>
> server :: Worker MyApi
> server = getBooks
>   where getBooks :: Handler [Book]
>         getBooks = ...return all books...
-}
instance
  (AtMostOneFragment api, FragmentUnique (Fragment a1 :> api), HasWorker e api context) =>
  HasWorker e (Fragment a1 :> api) context
  where
  type WorkerT e (Fragment a1 :> api) m = WorkerT e api m

  route pe _ = route pe (Proxy :: Proxy api)

  hoistWorkerWithContext pe _ = hoistWorkerWithContext pe (Proxy :: Proxy api)

{- $setup
>>> import Servant
-}

-- | A type that specifies that an API record contains a server implementation.
data AsWorkerT (e :: Prototype) (m :: Type -> Type)

instance GenericMode (AsWorkerT e m) where
  type AsWorkerT e m :- api = WorkerT e api m

type AsWorker e = AsWorkerT e (Handler e)

-- | Set of constraints required to convert to / from vanilla server types.
type GWorkerConstraints e api m =
  ( ToServant api (AsWorkerT e m) ~ WorkerT e (ToServantApi api) m
  , GServantProduct (Rep (api (AsWorkerT e m)))
  )

{- | This class is a necessary evil: in the implementation of 'HasWorker' for
 @'NamedRoutes' api@, we essentially need the quantified constraint @forall
 m. 'GWorkerConstraints' m@ to hold.

We cannot require do that directly as the definition of 'GWorkerConstraints'
contains type family applications ('Rep' and 'WorkerT'). The trick is to hide
those type family applications behind a typeclass providing evidence for
@'GWorkerConstraints' api m@ in the form of a dictionary, and require that
@forall m. 'GWorker' api m@ instead.

Users shouldn't have to worry about this class, as the only possible instance
is provided in this module for all record APIs.
-}
class GWorker (e :: Prototype) (api :: Type -> Type) (m :: Type -> Type) where
  gServerProof :: Dict (GWorkerConstraints e api m)

instance
  ( ToServant api (AsWorkerT e m) ~ WorkerT e (ToServantApi api) m
  , GServantProduct (Rep (api (AsWorkerT e m)))
  ) =>
  GWorker e api m
  where
  gServerProof = Dict

instance
  ( HasWorker e (ToServantApi api) context
  , forall m. Generic (api (AsWorkerT e m))
  , forall m. GWorker e api m
  , ErrorIfNoGeneric api
  ) =>
  HasWorker e (NamedRoutes api) context
  where
  type WorkerT e (NamedRoutes api) m = api (AsWorkerT e m)

  route pe _ ctx delayed =
    case gServerProof @e @api @(Handler e) of
      Dict -> route pe (Proxy @(ToServantApi api)) ctx (toServant <$> delayed)

  hoistWorkerWithContext ::
    forall m n.
    Proxy e ->
    Proxy (NamedRoutes api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    api (AsWorkerT e m) ->
    api (AsWorkerT e n)
  hoistWorkerWithContext pe _ pctx nat server =
    case (gServerProof @e @api @m, gServerProof @e @api @n) of
      (Dict, Dict) ->
        fromServant servantSrvN
        where
          servantSrvM :: WorkerT e (ToServantApi api) m =
            toServant server
          servantSrvN :: WorkerT e (ToServantApi api) n =
            hoistWorkerWithContext pe (Proxy @(ToServantApi api)) pctx nat servantSrvM
