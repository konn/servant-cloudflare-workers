{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Servant.Auth.Cloudflare.Workers.Internal.JWT (
  jwtAuthCheck,
  defaultCloudflareZeroTrustSettings,
  cloudflareZeroTrustAuthCheck,

  -- ** Mid-level functions
  verifyJWT,
  toCryptoKey,
  JWSAlg (..),
  CryptoKey,

  -- ** Low-level functions
  detectAlgorithm,
  verifySignature,
  Signature,
  Message,

  -- * Re-exports
  module Servant.Auth.JWT,
  module Servant.Auth.Cloudflare.Workers.Internal.ConfigTypes,

  -- * Misc functions
  toVerificationAlgorithm,
  toAlogirhtmIdentifier,
) where

import Control.Exception.Safe (throwString)
import Control.Monad (MonadPlus (..), guard, unless, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Reader
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Parser as AA
import Data.Aeson.Types (ToJSON)
import qualified Data.Attoparsec.ByteString.Streaming as AQ
import qualified Data.Bifunctor as Bi
import Data.Bitraversable (bitraverse)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Foldable (forM_)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Traversable (forM)
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.AlgorithmIdentifier (AlgorithmIdentifier)
import GHC.Wasm.Web.Generated.CryptoKey (CryptoKey)
import qualified GHC.Wasm.Web.Generated.CryptoKey as CryptoKey
import GHC.Wasm.Web.Generated.JsonWebKey
import qualified GHC.Wasm.Web.Generated.Response as Resp
import GHC.Wasm.Web.Generated.SubtleCrypto (js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any, js_fun_verify_AlgorithmIdentifier_CryptoKey_BufferSource_BufferSource_Promise_any)
import GHC.Wasm.Web.JSON (decodeJSON, encodeJSON)
import GHC.Wasm.Web.ReadableStream (fromReadableStream)
import Network.Cloudflare.Worker.Crypto (subtleCrypto)
import qualified Network.Cloudflare.Worker.FetchAPI as Fetch
import qualified Network.Cloudflare.Worker.Request as Req
import Servant.Auth.Cloudflare.Workers.Internal.ConfigTypes
import Servant.Auth.Cloudflare.Workers.Internal.Types
import Servant.Auth.JWT
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import qualified Streaming.ByteString as Q
import System.IO.Unsafe (unsafePerformIO)
import qualified Wasm.Prelude.Linear as PL

{- |
A @AuthCheck@ for Cloudflare ZeroTrust. You likely won't need to use this directly unless you are protecting a @Raw@ endpoint.
-}
cloudflareZeroTrustAuthCheck ::
  (FromJWT usr, HasCallStack) =>
  CloudflareZeroTrustSettings ->
  AuthCheck usr
cloudflareZeroTrustAuthCheck sett = do
  let setts = toJWTSettings sett
  req <- ask
  token <- maybe (liftIO $ throwString "No JWT token found") pure $ do
    lookup "Cf-Access-Jwt-Assertion" $
      map (Bi.first CI.mk) $
        Req.getHeaders req.rawRequest
  liftIO $ either throwString pure =<< verifyJWT setts token

{- | A JWT @AuthCheck@. You likely won't need to use this directly unless you
are protecting a @Raw@ endpoint.
-}
jwtAuthCheck :: (FromJWT usr) => JWTSettings -> AuthCheck usr
jwtAuthCheck jwtSettings = do
  req <- ask
  token <- maybe mempty pure $ do
    authHdr <- lookup "Authorization" $ Req.getHeaders req.rawRequest
    let bearer = "Bearer "
        (mbearer, rest) = BS.splitAt (BS.length bearer) authHdr
    guard (mbearer == bearer)
    pure rest
  verifiedJWT <- liftIO $ verifyJWT jwtSettings token
  case verifiedJWT of
    Left {} -> mzero
    Right v -> pure v

type RawTokenHeader = BS.ByteString

type RawTokenPayload = BS.ByteString

data RawJWTToken = RawJWTToken
  { header :: RawTokenHeader
  , decodedHeader :: BS.ByteString
  , payload :: RawTokenPayload
  , decodedPayload :: BS.ByteString
  , signature :: Signature
  }
  deriving (Show, Eq, Ord, Generic)

decodeB64Pad :: BS8.ByteString -> Either String Signature
decodeB64Pad = B64.decode . pad
  where
    pad bs =
      let n = BS.length bs
          pads = BS8.replicate ((-n) `rem` 4) '='
       in bs <> pads

parseJWT :: BS.ByteString -> Either String RawJWTToken
parseJWT raw = Bi.first (("Error during parsing token (" <> BS8.unpack raw <> "):") <>) $
  case BS8.split '.' raw of
    [header, payload, sigB64] -> do
      decodedHeader <-
        Bi.first ("Invalid Header (Base65): " <>) $ decodeB64Pad header
      Bi.first ("Invalid Header: " <>) $ validateRawJSON decodedHeader
      decodedPayload <-
        Bi.first ("Invalid payload (Base64): " <>) (decodeB64Pad payload)
      Bi.first ("Invalid Payload: " <>) $ validateRawJSON decodedPayload
      signature <- Bi.first ("Invalid signature (Base64): " <>) $ decodeB64Pad sigB64

      pure RawJWTToken {..}
    _ -> Left "Invalid JWT String"

validateRawJSON :: BS.ByteString -> Either String ()
validateRawJSON raw =
  if BS8.any (`elem` ("\r\n\t " :: [Char])) raw
    then Left "JSON contains whitespaces"
    else Right ()

verifyJWT :: (FromJWT a) => JWTSettings -> BS.ByteString -> IO (Either String a)
verifyJWT jwtCfg input = runExceptT do
  now <- liftIO getPOSIXTime
  rawJWT <-
    ExceptT $
      pure $
        verifyJWTWith jwtCfg.validationKeys now
          =<< parseJWT input
  verified <- verifyClaims jwtCfg rawJWT
  ExceptT $ pure $ Bi.first T.unpack $ decodeJWT verified

verifyClaims :: JWTSettings -> JWT -> ExceptT String IO ClaimsSet
verifyClaims jwtCfg jwt = do
  let audMatches = jwtCfg.audienceMatches
  liftIO $ do
    forM_ jwt.payload.aud \(Audiences auds) ->
      unless (any ((== Matches) . audMatches) auds) do
        fail "Invalid audience"
  pure jwt.payload

data JWT = JWT
  { header :: JWSHeader
  , payload :: ClaimsSet
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

verifyJWTWith ::
  [(Maybe T.Text, CryptoKey)] ->
  POSIXTime ->
  RawJWTToken ->
  Either String JWT
verifyJWTWith keys now toks = do
  header <-
    Bi.first ("Invalid Header (JSON): " <>) $
      J.eitherDecodeStrict' toks.decodedHeader
  payload <-
    Bi.first ("Invalid Payload (JSON): " <>) $
      J.eitherDecodeStrict' toks.decodedPayload
  let msg = toks.header <> "." <> toks.payload
  ok <- case header.kid of
    Just kid -> do
      key <- maybe (Left $ "Key id not found: " <> show header.kid) pure $ lookup (Just kid) keys
      pure $ verifySignature key toks.signature msg
    Nothing ->
      pure $ any (\k -> verifySignature k toks.signature msg) (map snd keys)
  unless ok $
    Left "Invalid signature"
  verifyTimestamps now payload
  pure JWT {..}

verifyTimestamps :: POSIXTime -> ClaimsSet -> Either String ()
verifyTimestamps now pay = do
  when (maybe False (> NumericDate now) pay.iat) do
    Left "Token issued in the future"
  when (maybe False (NumericDate now >) pay.exp) do
    Left "Token expired"
  when (maybe False (NumericDate now <) pay.nbf) do
    Left "Token not yet valid"

type Signature = BS.ByteString

type Message = BS.ByteString

toAlogirhtmIdentifier :: JWSAlg -> AlgorithmIdentifier
toAlogirhtmIdentifier = \case
  HS256 ->
    inject $
      newDictionary
        @( '[ '("name", DOMStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "HMAC"
        PL.. setPartialField "hash" "SHA-256"
  HS384 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "HMAC"
        PL.. setPartialField "hash" "SHA-384"
  HS512 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "HMAC"
        PL.. setPartialField "hash" "SHA-512"
  RS256 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "RSASSA-PKCS1-v1_5"
        PL.. setPartialField "hash" "SHA-256"
  RS384 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "RSASSA-PKCS1-v1_5"
        PL.. setPartialField "hash" "SHA-384"
  RS512 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "RSASSA-PKCS1-v1_5"
        PL.. setPartialField "hash" "SHA-512"
  ES256 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("namedCurve", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "ECDSA"
        PL.. setPartialField "namedCurve" "P-256"
  ES384 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("namedCurve", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "ECDSA"
        PL.. setPartialField "namedCurve" "P-384"
  ES512 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("namedCurve", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "ECDSA"
        PL.. setPartialField "namedCurve" "P-521"
  PS256 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "RSA-PSS"
        PL.. setPartialField "hash" "SHA-256"
  PS384 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "RSA-PSS"
        PL.. setPartialField "hash" "SHA-384"
  PS512 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "RSA-PSS"
        PL.. setPartialField "hash" "SHA-512"
  EdDSA ->
    inject $
      newDictionary
        @'[ '("name", USVStringClass)]
        PL.$ setPartialField "name" "Ed25519"

verifySignature :: CryptoKey -> Signature -> Message -> Bool
verifySignature key sig msg = unsafePerformIO do
  useByteStringAsJSByteArray @Word8 sig \sig' ->
    useByteStringAsJSByteArray @Word8 msg \msg' ->
      fmap (fromJSPrim . unsafeCast @_ @(JSPrimClass Bool)) . await
        =<< js_fun_verify_AlgorithmIdentifier_CryptoKey_BufferSource_BufferSource_Promise_any
          subtleCrypto
          (toVerificationAlgorithm $ fromMaybe (error "Could not determine algorithm") $ detectAlgorithm key)
          key
          (inject sig')
          (inject msg')

toVerificationAlgorithm :: JWSAlg -> AlgorithmIdentifier
toVerificationAlgorithm = \case
  HS256 -> inject @USVStringClass "HMAC"
  HS384 -> inject @USVStringClass "HMAC"
  HS512 -> inject @USVStringClass "HMAC"
  RS256 -> inject @USVStringClass "RSASSA-PKCS1-v1_5"
  RS384 -> inject @USVStringClass "RSASSA-PKCS1-v1_5"
  RS512 -> inject @USVStringClass "RSASSA-PKCS1-v1_5"
  ES256 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "ECDSA"
        PL.. setPartialField "hash" "SHA-256"
  ES384 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "ECDSA"
        PL.. setPartialField "hash" "SHA-384"
  ES512 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("hash", USVStringClass)
            ]
         )
        PL.$ setPartialField "name" "ECDSA"
        PL.. setPartialField "hash" "SHA-512"
  PS256 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("saltLength", JSPrimClass Word16)
            ]
         )
        PL.$ setPartialField "name" "RSA-PSS"
        PL.. setPartialField "saltLength" (toJSPrim 32)
  PS384 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("saltLength", JSPrimClass Word16)
            ]
         )
        PL.$ setPartialField "name" "RSA-PSS"
        PL.. setPartialField "saltLength" (toJSPrim 48)
  PS512 ->
    inject $
      newDictionary
        @( '[ '("name", USVStringClass)
            , '("saltLength", JSPrimClass Word16)
            ]
         )
        PL.$ setPartialField "name" "RSA-PSS"
        PL.. setPartialField "saltLength" (toJSPrim 64)
  EdDSA ->
    inject $
      newDictionary
        @'[ '("name", USVStringClass)]
        PL.$ setPartialField "name" "Ed25519"

toCryptoKey :: JWSAlg -> JsonWebKey -> IO CryptoKey
toCryptoKey alg jwk =
  fmap unsafeCast . await
    =<< js_fun_importKey_KeyFormat_object_AlgorithmIdentifier_boolean_sequence_KeyUsage_Promise_any
      subtleCrypto
      "jwk"
      (upcast jwk)
      (toAlogirhtmIdentifier alg)
      False
      (toSequence $ pure "verify")

detectAlgorithm :: CryptoKey -> Maybe JWSAlg
detectAlgorithm key = do
  !rawAlg <- unsafePerformIO $ decodeJSON @AlgorithmParams =<< CryptoKey.js_get_algorithm key
  case rawAlg.name of
    "HMAC" -> do
      hash <- rawAlg.hash
      case hash of
        "SHA-256" -> Just HS256
        "SHA-384" -> Just HS384
        "SHA-512" -> Just HS512
        _ -> Nothing
    "ECDSA" -> do
      namedCurve <- rawAlg.namedCurve
      case namedCurve of
        "P-256" -> Just ES256
        "P-384" -> Just ES384
        "P-521" -> Just ES512
        _ -> Nothing
    "RSASSA-PKCS1-v1_5" -> do
      hash <- rawAlg.hash
      case hash of
        "SHA-256" -> Just RS256
        "SHA-384" -> Just RS384
        "SHA-512" -> Just RS512
        _ -> Nothing
    "RSA-PSS" -> do
      hash <- rawAlg.hash
      case hash of
        "SHA-256" -> Just PS256
        "SHA-384" -> Just PS384
        "SHA-512" -> Just PS512
        _ -> Nothing
    _ -> Nothing

data AlgorithmParams = AlgorithmParams
  { name :: T.Text
  , modulusLength :: !(Maybe Word)
  , publicExponent :: !(Maybe Word)
  , hash :: !(Maybe T.Text)
  , namedCurve :: !(Maybe T.Text)
  , length :: !Word
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

newtype Keys = Keys {keys :: HashMap T.Text J.Value}
  deriving (Generic)
  deriving anyclass (J.FromJSON)

defaultCloudflareZeroTrustSettings ::
  AudienceId ->
  TeamName ->
  IO CloudflareZeroTrustSettings
defaultCloudflareZeroTrustSettings cfAudienceId teamName = do
  rsp <-
    await
      =<< Fetch.get ("https://" <> teamName <> ".cloudflareaccess.com/cdn-cgi/access/certs")
  val <-
    either throwString (mapM encodeJSON . (.keys))
      . eitherResult
      . fromJSON @Keys
      . fst
      =<< bitraverse (either (throwString . show) pure) Q.effects
      =<< maybe
        (throwString "Empty Body returned for Cloudflare certs!")
        (AQ.parse AA.json' . fromReadableStream)
        . fromNullable
      =<< Resp.js_get_body rsp
  cfValidationKeys <- forM val $ toCryptoKey RS256 . unsafeCast
  pure CloudflareZeroTrustSettings {..}
