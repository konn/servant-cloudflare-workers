{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.API.Cloudflare (
  ReadableStreamBody,
) where

import Data.Kind
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Servant.API

{- | A request body, that is passed as a @ReadableStream@ on worker side,
and given by any type on the client side.
-}
data ReadableStreamBody (contentTypes :: [Type]) (a :: Type)
  deriving (Generic)

instance (HasLink sub) => HasLink (ReadableStreamBody contentTypes a :> sub) where
  type MkLink (ReadableStreamBody contentTypes a :> sub) x = MkLink sub x
  toLink toA _ = toLink toA (Proxy @sub)
  {-# INLINE toLink #-}
