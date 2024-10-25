{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Auth.Cloudflare.Workers.Internal.ThrowAll where

import Control.Monad.Error.Class
import qualified Data.ByteString.Char8 as BS
import Network.Cloudflare.Worker.Handler.Fetch (FetchContext)
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Network.HTTP.Types
import Servant.API.Generic
import Servant.Cloudflare.Workers
import Servant.Cloudflare.Workers.Generic
import Servant.Cloudflare.Workers.Internal.Response (responseBS, toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.RoutingApplication (RoutingRequest)
import Servant.Cloudflare.Workers.Prelude ((:<|>) (..))

class ThrowAll a where
  -- | 'throwAll' is a convenience function to throw errors across an entire
  -- sub-API
  --
  --
  -- > throwAll err400 :: Handler a :<|> Handler b :<|> Handler c
  -- >    == throwError err400 :<|> throwError err400 :<|> err400
  throwAll :: ServerError -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
  throwAll e = throwAll e :<|> throwAll e

instance
  (ThrowAll (ToServant api (AsWorkerT m)), GenericServant api (AsWorkerT m)) =>
  ThrowAll (api (AsWorkerT m))
  where
  throwAll = fromServant . throwAll

-- Really this shouldn't be necessary - ((->) a) should be an instance of
-- MonadError, no?
instance {-# OVERLAPPING #-} (ThrowAll b) => ThrowAll (a -> b) where
  throwAll e = const $ throwAll e

instance {-# OVERLAPPABLE #-} (MonadError ServerError m) => ThrowAll (m a) where
  throwAll = throwError

-- | for @servant >=0.11@
instance {-# OVERLAPPING #-} (MonadError ServerError m) => ThrowAll (Tagged m (RoutingRequest -> JSObject e -> FetchContext -> IO WorkerResponse)) where
  throwAll e = Tagged $ \_req _ _ ->
    toWorkerResponse $
      responseBS
        (mkStatus (errHTTPCode e) (BS.pack $ errReasonPhrase e))
        (errHeaders e)
        (errBody e)
