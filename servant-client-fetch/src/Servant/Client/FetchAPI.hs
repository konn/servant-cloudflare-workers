{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Client.FetchAPI (
  FetchT (),
  FetchM,
  runFetch,
  runFetchWith,
  module Servant.Client.Core.Reexport,
) where

import Control.Exception (throwIO)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, SomeException (..), throwString)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Reader
import Data.Bifunctor qualified as Bi
import Data.Bitraversable qualified as Bi
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Proxy
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Prim
import GHC.Wasm.Web.Generated.BodyInit.Core (BodyInit)
import GHC.Wasm.Web.Generated.Headers qualified as Hdrs
import GHC.Wasm.Web.Generated.RequestInfo
import GHC.Wasm.Web.Generated.RequestInit
import GHC.Wasm.Web.Generated.Response
import GHC.Wasm.Web.Generated.Response qualified as FetchResp
import GHC.Wasm.Web.Generated.Response qualified as JS
import GHC.Wasm.Web.Generated.URL
import GHC.Wasm.Web.ReadableStream (ReadableStream, fromReadableStream)
import Lens.Family.Total
import Network.HTTP.Media (MediaType, renderHeader)
import Network.HTTP.Types.Status (Status (..))
import Network.HTTP.Types.URI (renderQuery)
import Servant.API hiding (inject)
import Servant.API.Cloudflare
import Servant.API.ContentTypes (AllMime (..))
import Servant.Client.Core
import Servant.Client.Core qualified as Servant
import Servant.Client.Core.Reexport
import Servant.Types.SourceT (SourceT (..))
import Servant.Types.SourceT qualified as Servant
import Streaming.ByteString qualified as Q
import Streaming.ByteString.Internal qualified as QI
import Streaming.Prelude qualified as S

newtype FetchT m a = FetchT (ReaderT FetchEnv m a)
  deriving (Functor)
  deriving newtype (MonadThrow, MonadCatch, MonadUnliftIO, MonadMask, Applicative, Monad, MonadIO)
  deriving (Semigroup, Monoid) via Ap (FetchT m) a

type FetchM = FetchT IO

data FetchEnv = FetchEnv {fetcher :: !Fetcher, baseUrl :: !BaseUrl}
  deriving (Generic)

runFetch :: BaseUrl -> FetchT m a -> m a
runFetch burl (FetchT act) =
  runReaderT
    act
    FetchEnv
      { fetcher = js_toplevel_fetch
      , baseUrl = burl
      }

runFetchWith :: JSObject cls -> BaseUrl -> FetchT m a -> m a
runFetchWith obj burl (FetchT act) =
  runReaderT
    act
    FetchEnv
      { fetcher = js_fetch_of obj
      , baseUrl = burl
      }

instance (MonadIO m) => RunClient (FetchT m) where
  throwClientError = liftIO . throwIO
  runRequestAcceptStatus _ req = do
    FetchEnv {..} <- FetchT ask
    liftIO do
      either throwIO pure
        =<< fetchWith fetcher baseUrl req

toUint8Array :: BS.ByteString -> IO (JSByteArray Word8)
toUint8Array bs = useByteStringAsJSByteArray @Word8 bs cloneByteArray

fromBody :: Servant.RequestBody -> IO BodyInit
fromBody (Servant.RequestBodyLBS lbs) =
  inject <$> toUint8Array (LBS.toStrict lbs)
fromBody (Servant.RequestBodyBS bs) =
  inject <$> toUint8Array bs
fromBody (Servant.RequestBodySource src) =
  fmap inject $ toUint8Array =<< Q.toStrict_ (fromSourceIOLBS src)

fromSourceIOLBS :: SourceIO LBS.ByteString -> Q.ByteStream IO ()
fromSourceIOLBS (Servant.SourceT withSteps) = QI.Go $ withSteps $ pure . go
  where
    go :: Servant.StepT IO LBS.ByteString -> Q.ByteStream IO ()
    go = \case
      Servant.Stop -> mempty
      Servant.Yield lbs stp -> Q.fromLazy lbs >> go stp
      Servant.Error err -> liftIO $ throwString err
      Servant.Effect es -> QI.Go $ go <$> es
      Servant.Skip a -> go a

fromResp :: HttpVersion -> FetchResp.Response -> IO Servant.Response
fromResp ver resp = do
  statusCode <- fromIntegral <$> FetchResp.js_get_status resp
  statusMessage <- toHaskellByteString =<< FetchResp.js_get_statusText resp
  responseHeaders <-
    fmap Seq.fromList
      . S.toList_
      . S.mapM (Bi.bitraverse (fmap CI.mk . toHaskellByteString) toHaskellByteString)
      . fromPairIterable
      =<< Hdrs.js_iter_Headers_ByteString_ByteString
      =<< FetchResp.js_get_headers resp
  mbody <- fromNullable <$> FetchResp.js_get_body resp
  let responseStatusCode = Status {..}
      responseHttpVersion = ver
  responseBody <- case mbody of
    Nothing -> pure mempty
    Just rst -> Q.toLazy_ $ fromReadableStream rst
  pure Servant.Response {..}

type Fetcher =
  RequestInfo ->
  Nullable RequestInitClass ->
  IO (Promise ResponseClass)

data FetchResult = Ok JS.Response | StatusError JS.Response | UnknownError T.Text
  deriving (Generic)

instance
  ( RunClient m
  , AllMime ctypes
  , HasClient m api
  , ToSourceIO LBS.ByteString a
  ) =>
  HasClient m (ReadableStreamBody ctypes a :> api)
  where
  type Client m (ReadableStreamBody ctypes a :> api) = (MediaType, a) -> Client m api

  hoistClientMonad pm _ f cl = \a ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl a)

  clientWithRoute pm Proxy req (ct, body) =
    hoistClientMonad @m @_ @m @m
      pm
      (Proxy :: Proxy api)
      ( \act -> do
          unless (ct `elem` mimes) $ do
            throwClientError $ ConnectionError $ SomeException $ userError "Content-Type is not supported by the server"
          act
      )
      $ clientWithRoute pm (Proxy :: Proxy api)
      $ setRequestBody
        (RequestBodySource sourceIO)
        ct
        req
    where
      mimes = allMime $ Proxy @ctypes
      sourceIO = toSourceIO body

instance ToSourceIO BS.ByteString ReadableStream where
  toSourceIO rbs = do
    SourceT \withStep ->
      withStep $ go $ fromReadableStream rbs
    where
      go :: Q.ByteStream IO () -> Servant.StepT IO BS.ByteString
      go = \case
        QI.Chunk bs a -> Servant.Yield bs $ go a
        QI.Empty {} -> Servant.Stop
        QI.Go act -> Servant.Effect $ act <&> go

fetchWith ::
  Fetcher ->
  BaseUrl ->
  Request ->
  IO (Either ClientError Servant.Response)
fetchWith fetcher baseUrl req = do
  let path =
        fromText $
          TE.decodeUtf8 $
            LBS.toStrict $
              BB.toLazyByteString req.requestPath
      base = toUSVString $ toJSString $ showBaseUrl baseUrl
  url <- liftIO $ js_cons_URL path $ nonNull base
  liftIO $ do
    unless (null req.requestQueryString) $
      js_set_search url $
        fromText $
          TE.decodeUtf8 $
            renderQuery True $
              F.toList req.requestQueryString
  meth <- fromHaskellByteString req.requestMethod
  let headerSeeds =
        addCType $
          addAccept $
            filter ((`notElem` ["Content-Type", "Accept"]) . fst) $
              F.toList req.requestHeaders
      addAccept
        | null req.requestAccept = id
        | otherwise = (("Accept", renderHeader $ F.toList req.requestAccept) :)
      addCType = maybe id ((:) . ("Content-Type",) . renderHeader . snd) req.requestBody
  hdrs <-
    toJSRecord @JSByteStringClass @JSByteStringClass
      . Map.fromList
      =<< mapM
        (Bi.bitraverse (pure . TE.decodeUtf8 . CI.original) fromHaskellByteString)
        headerSeeds
  mbody <- mapM (fromBody . fst) req.requestBody
  -- NOTE: We once used newDicationary, but it seems its purity makes GHC optimiser
  -- work wrong and makes consecutive calls to reuse body from the previous request.
  -- This must not be the case, so we provide a direct reqinit construction to avoid
  -- the bug.
  reqInit <- newReqInit meth hdrs mbody
  res <- await =<< js_handle_fetch =<< fetcher (unsafeCast url) (nonNull reqInit)
  resl <- getDictField "result" res
  resl
    & ( _case
          & onEnum #ok do
            mresp <- fromNullable <$> getDictField "response" res
            case mresp of
              Nothing -> pure $ Left $ ConnectionError $ SomeException $ userError "invariant violation: ok returned, but got no response!"
              Just resp -> Right <$> fromResp req.requestHttpVersion resp
          & onEnum #statusError do
            mresp <- fromNullable <$> getDictField "response" res
            case mresp of
              Just resp ->
                Left
                  . FailureResponse (Bi.bimap (const ()) (\u -> (baseUrl, LBS.toStrict $ BB.toLazyByteString u)) req)
                  <$> fromResp req.requestHttpVersion resp
              Nothing -> pure $ Left $ ConnectionError $ SomeException $ userError "invariant violation: statusError returned, but got no response!"
          & onEnum #error do
            Left
              . ConnectionError
              . SomeException
              . userError
              . ("UnknownError during fetch: " <>)
              . nullable "(No Message)" (T.unpack . toText)
              <$> getDictField "message" res
      )

newReqInit :: JSByteString -> JSRecord JSByteStringClass JSByteStringClass -> Maybe BodyInit -> IO RequestInit
newReqInit meth hdrs mbody = do
  case mbody of
    Nothing -> js_new_req_init_nobody meth hdrs
    Just body -> js_new_req_nobody meth hdrs body

type FetchResultFields =
  '[ '("result", EnumClass '["ok", "statusError", "error"])
   , '("response", NullableClass ResponseClass)
   , '("message", NullableClass USVStringClass)
   ]

type FetchResultClass = JSDictionaryClass FetchResultFields

foreign import javascript safe "try { const resp = await $1; if (resp.ok) { return {result: 'ok', response: resp, message: null } } else { return {result:  'statusError', response: resp, message: resp.statusText} } } catch (error) { return {result: 'error', message: error.toString(), response: null } }"
  js_handle_fetch ::
    Promise ResponseClass ->
    IO (Promise FetchResultClass)

foreign import javascript safe "fetch($1, $2)"
  js_toplevel_fetch :: Fetcher

foreign import javascript safe "$1.fetch($2, $3)"
  js_fetch_of :: JSObject a -> Fetcher

foreign import javascript unsafe "new Uint8Array($1)"
  cloneByteArray :: JSByteArray c -> IO (JSByteArray c)

foreign import javascript unsafe "{ method: $1, headers: $2 }"
  js_new_req_init_nobody :: JSByteString -> JSRecord JSByteStringClass JSByteStringClass -> IO RequestInit

foreign import javascript unsafe "{ method: $1, headers: $2, body: $3 }"
  js_new_req_nobody :: JSByteString -> JSRecord JSByteStringClass JSByteStringClass -> BodyInit -> IO RequestInit
