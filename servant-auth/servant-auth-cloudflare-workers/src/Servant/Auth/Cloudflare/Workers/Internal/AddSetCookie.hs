{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Auth.Cloudflare.Workers.Internal.AddSetCookie where

import Blaze.ByteString.Builder (toByteString)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import GHC.Wasm.Object.Builtins (fromHaskellByteString)
import GHC.Wasm.Web.Generated.Headers (js_fun_append_ByteString_ByteString_undefined)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import qualified Network.Cloudflare.Worker.Response as Resp
import qualified Network.HTTP.Types as HTTP
import Servant.API.Generic
import Servant.Cloudflare.Workers.Generic
import Servant.Cloudflare.Workers.Prelude
import Web.Cookie

-- What are we doing here? Well, the idea is to add headers to the response,
-- but the headers come from the authentication check. In order to do that, we
-- tweak a little the general theme of recursing down the API tree; this time,
-- we recurse down a variation of it that adds headers to all the endpoints.
-- This involves the usual type-level checks.
--
-- TODO: If the endpoints already have headers, this will not work as is.

data Nat = Z | S Nat

type family AddSetCookiesApi (n :: Nat) a where
  AddSetCookiesApi ('S 'Z) a = AddSetCookieApi a
  AddSetCookiesApi ('S n) a = AddSetCookiesApi n (AddSetCookieApi a)

type family AddSetCookieApiVerb a where
  AddSetCookieApiVerb (Headers ls a) = Headers (Header "Set-Cookie" SetCookie ': ls) a
  AddSetCookieApiVerb a = Headers '[Header "Set-Cookie" SetCookie] a

type family MapAddSetCookieApiVerb (as :: [Type]) where
  MapAddSetCookieApiVerb '[] = '[]
  MapAddSetCookieApiVerb (a ': as) = (AddSetCookieApiVerb a ': MapAddSetCookieApiVerb as)

type family AddSetCookieApi a :: Type

type instance AddSetCookieApi (a :> b) = a :> AddSetCookieApi b

type instance AddSetCookieApi (a :<|> b) = AddSetCookieApi a :<|> AddSetCookieApi b

type instance AddSetCookieApi (NamedRoutes api) = AddSetCookieApi (ToServantApi api)

type instance
  AddSetCookieApi (Verb method stat ctyps a) =
    Verb method stat ctyps (AddSetCookieApiVerb a)

type instance
  AddSetCookieApi (UVerb method ctyps as) =
    UVerb method ctyps (MapAddSetCookieApiVerb as)

type instance AddSetCookieApi Raw = Raw

type instance
  AddSetCookieApi (Stream method stat framing ctyps a) =
    Stream method stat framing ctyps (AddSetCookieApiVerb a)

type instance AddSetCookieApi (Headers hs a) = AddSetCookieApiVerb (Headers hs a)

data SetCookieList (n :: Nat) :: Type where
  SetCookieNil :: SetCookieList 'Z
  SetCookieCons :: Maybe SetCookie -> SetCookieList n -> SetCookieList ('S n)

class AddSetCookies (n :: Nat) orig new where
  addSetCookies :: SetCookieList n -> orig -> new

instance
  {-# OVERLAPS #-}
  (AddSetCookies ('S n) oldb newb) =>
  AddSetCookies ('S n) (a -> oldb) (a -> newb)
  where
  addSetCookies cookies oldfn = addSetCookies cookies . oldfn

instance (orig1 ~ orig2) => AddSetCookies 'Z orig1 orig2 where
  addSetCookies _ = id

instance
  {-# OVERLAPPABLE #-}
  ( Functor m
  , AddSetCookies n (m old) (m cookied)
  , AddHeader mods "Set-Cookie" SetCookie cookied new
  ) =>
  AddSetCookies ('S n) (m old) (m new)
  where
  addSetCookies (mCookie `SetCookieCons` rest) oldVal =
    case mCookie of
      Nothing -> noHeader' <$> addSetCookies rest oldVal
      Just cookie -> addHeader' cookie <$> addSetCookies rest oldVal

instance
  {-# OVERLAPS #-}
  (AddSetCookies ('S n) a a', AddSetCookies ('S n) b b') =>
  AddSetCookies ('S n) (a :<|> b) (a' :<|> b')
  where
  addSetCookies cookies (a :<|> b) = addSetCookies cookies a :<|> addSetCookies cookies b

instance
  {-# OVERLAPPING #-}
  (AddSetCookies ('S n) a a, AddSetCookies ('S n) b b') =>
  AddSetCookies ('S n) (a :<|> b) (a :<|> b')
  where
  addSetCookies cookies (a :<|> b) = addSetCookies cookies a :<|> addSetCookies cookies b

instance
  {-# OVERLAPS #-}
  ( AddSetCookies ('S n) (WorkerT e (ToServantApi api) m) cookiedApi
  , Generic (api (AsWorkerT e m))
  , GServantProduct (Rep (api (AsWorkerT e m)))
  , ToServant api (AsWorkerT e m) ~ WorkerT e (ToServantApi api) m
  ) =>
  AddSetCookies ('S n) (api (AsWorkerT e m)) cookiedApi
  where
  addSetCookies cookies = addSetCookies cookies . toServant

-- | for @servant >=0.11@
instance AddSetCookies ('S n) (Tagged m (FetchHandler e)) (Tagged m (FetchHandler e)) where
  addSetCookies cookies r = Tagged $ \request jsenv fctx ->
    addHeaders (mkHeaders cookies) =<< unTagged r request jsenv fctx

addHeaders :: [HTTP.Header] -> WorkerResponse -> IO WorkerResponse
addHeaders newHdrs resp = do
  hdrs <- Resp.getHeaders resp
  forM_ newHdrs \(hdr, val) -> do
    hdr' <- fromHaskellByteString $ CI.original hdr
    val' <- fromHaskellByteString val
    js_fun_append_ByteString_ByteString_undefined hdrs hdr' val'
  pure resp

mkHeaders :: SetCookieList x -> [HTTP.Header]
mkHeaders x = ("Set-Cookie",) <$> mkCookies x
  where
    mkCookies :: forall y. SetCookieList y -> [BS.ByteString]
    mkCookies SetCookieNil = []
    mkCookies (SetCookieCons Nothing rest) = mkCookies rest
    mkCookies (SetCookieCons (Just y) rest) =
      toByteString (renderSetCookie y) : mkCookies rest
