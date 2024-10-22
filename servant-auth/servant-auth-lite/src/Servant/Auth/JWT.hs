{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Servant.Auth.JWT (
  ClaimsSet (..),
  Audiences (..),
  NumericDate (..),
  FromJWT (..),
  ToJWT (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson (
  FromJSON (..),
  Result (..),
  ToJSON,
  Value,
  fromJSON,
  toJSON,
  withObject,
 )
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types ((.:?))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import GHC.IsList (IsList)

newtype Audiences = Audiences {getAudiences :: [T.Text]}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsList, Semigroup, Monoid)

instance FromJSON Audiences where
  parseJSON obj =
    (Audiences <$> parseJSON obj)
      <|> (Audiences . pure <$> parseJSON obj)

instance ToJSON Audiences where
  toJSON (Audiences xs) = toJSON xs

newtype NumericDate = NumericDate {posixDate :: POSIXTime}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

-- This should probably also be from ClaimSet
--

registeredClaims :: S.Set T.Text
registeredClaims =
  S.fromDistinctAscList
    [ "aud"
    , "exp"
    , "iat"
    , "iss"
    , "jti"
    , "nbf"
    , "sub"
    ]

data ClaimsSet = ClaimsSet
  { iss :: Maybe T.Text
  , sub :: Maybe T.Text
  , aud :: Maybe Audiences
  , exp :: Maybe NumericDate
  , nbf :: Maybe NumericDate
  , iat :: Maybe NumericDate
  , jti :: Maybe T.Text
  , unregisteredClaims :: M.Map T.Text Value
  }
  deriving (Show, Eq, Ord, Generic)

-- | Collects all 'aud's and favours first non-null values for other claims.
instance Semigroup ClaimsSet where
  a <> b =
    ClaimsSet
      { iss = a.iss <|> b.iss
      , sub = a.sub <|> b.sub
      , aud = a.aud <> b.aud
      , exp = a.exp <|> b.exp
      , nbf = a.nbf <|> b.nbf
      , iat = a.iat <|> b.iat
      , jti = a.jti <|> b.jti
      , unregisteredClaims = a.unregisteredClaims <> b.unregisteredClaims
      }

instance Monoid ClaimsSet where
  mempty =
    ClaimsSet
      { iss = Nothing
      , sub = Nothing
      , aud = Nothing
      , exp = Nothing
      , nbf = Nothing
      , iat = Nothing
      , jti = Nothing
      , unregisteredClaims = mempty
      }

instance FromJSON ClaimsSet where
  parseJSON = withObject "ClaimsSet" \o -> do
    iss <- o .:? "iss"
    sub <- o .:? "sub"
    aud <- o .:? "aud"
    exp_ <- o .:? "exp"
    nbf <- o .:? "nbf"
    iat <- o .:? "iat"
    jti <- o .:? "jti"
    unregisteredClaims <- pure $ KM.toMapText o `M.withoutKeys` registeredClaims
    pure ClaimsSet {exp = exp_, ..}

{- | How to decode data from a JWT.

The default implementation assumes the data is stored in the unregistered
@dat@ claim, and uses the @FromJSON@ instance to decode value from there.
-}
class FromJWT a where
  decodeJWT :: ClaimsSet -> Either T.Text a
  default decodeJWT :: (FromJSON a) => ClaimsSet -> Either T.Text a
  decodeJWT m = case M.lookup "dat" (m.unregisteredClaims) of
    Nothing -> Left "Missing 'dat' claim"
    Just v -> case fromJSON v of
      Error e -> Left $ T.pack e
      Success a -> Right a

{- | How to encode data from a JWT.

The default implementation stores data in the unregistered @dat@ claim, and
uses the type's @ToJSON@ instance to encode the data.
-}
class ToJWT a where
  encodeJWT :: a -> ClaimsSet
  default encodeJWT :: (ToJSON a) => a -> ClaimsSet
  encodeJWT a = mempty {unregisteredClaims = M.singleton "dat" (toJSON a)}
