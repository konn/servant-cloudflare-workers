{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (handlers, main) where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text
import GHC.Generics
import Prelude.Compat
import Servant.Cloudflare.Workers.Cache (CacheOptions (..), serveCached)
import Servant.Cloudflare.Workers.Generic ()
import Servant.Cloudflare.Workers.Prelude
import Prelude ()

-- * Example

-- | A greet message data type
newtype Greet = Greet {_msg :: Text}
  deriving (Generic, Show)

instance FromJSON Greet

instance ToJSON Greet

-- API specification
type TestApi =
  -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
  "hello"
    :> Capture "name" Text
    :> QueryParam "capital" Bool
    :> Get '[JSON] Greet
    -- POST /greet with a Greet as JSON in the request body,
    --             returns a Greet as JSON
    :<|> "greet"
    :> ReqBody '[JSON] Greet
    :> Post '[JSON] Greet
    -- DELETE /greet/:greetid
    :<|> "greet"
    :> Capture "greetid" Text
    :> Delete '[JSON] NoContent
    :<|> NamedRoutes OtherRoutes

data OtherRoutes mode = OtherRoutes
  { version :: mode :- Get '[JSON] Int
  , bye :: mode :- "bye" :> Capture "name" Text :> Get '[JSON] Text
  }
  deriving (Generic)

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Worker e TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH :<|> otherRoutes
  where
    otherRoutes = OtherRoutes {..}
    cacheOpts = CacheOptions {cacheTTL = 60, onlyOk = True, includeQuery = True}

    bye name = pure $ "Bye, " <> name <> " !"
    version = 42 <$ serveCached cacheOpts

    helloH :: Text -> Maybe Bool -> Handler e Greet
    helloH name mcapital = do
      serveCached cacheOpts
      if fromMaybe False mcapital
        then return . Greet . toUpper $ "Hello, " <> name
        else return . Greet $ "Hello, " <> name

    postGreetH greet = return greet

    deleteGreetH _ = return NoContent

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Cloudflare.Workers module.
handlers :: IO JSHandlers
handlers = compileWorker $ serve Proxy testApi server

foreign export javascript "handlers" handlers :: IO JSHandlers

-- Put this all to work!
main :: IO ()
main = pure ()
