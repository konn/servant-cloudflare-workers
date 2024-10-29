{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Servant.Cloudflare.Workers.R2 (
  -- * Methods
  head,
  head',
  get,
  get',
  getWith,
  getWith',
  RawGetOptions,
  GetOptionsClass,
  GetOptionsFields,
  put,
  put',
  PutBody,
  PutBodyClass,
  putWith,
  putWith',
  RawPutOptions,
  PutOptionsClass,
  PutOptionsFields,
  delete,
  delete',
  deleteMany,
  deleteMany',
  list,
  list',
  listRaw,
  listRaw',
  RawListOptions,
  R2ObjectsView (..),
  ListOptionsFields,
  RawListOptionsClass,
  R2ObjectsFields,
  R2ObjectsClass,
  R2Objects,

  -- * Object Metadata
  R2Object,
  R2ObjectClass,
  getObjectKey,
  getObjectVersion,
  getObjectSize,
  getObjectETagRaw,
  getObjectHTTPETag,
  getObjectCustomMetadata,
  RawStorageClass,
  StorageClass (..),
  RawStorageClassClass,
  fromRawStorageClass,
  toRawStorageClass,
  getObjectStorageClass,
  writeObjectHttpMetadata,

  -- * Object Body
  R2ObjectBody,
  getBody,
  isBodyUsed,
  getBodyArrayBuffer,
  getBodyText,
  getBodyBlob,

  -- * Http Metadata
  R2HTTPMetadata,
  R2HTTPMetadataClass,
  R2HTTPMetadataFields,

  -- * Ranged read data
  R2RangeClass,
  R2OffsetRangeClass,
  R2LengthRangeClass,
  R2SuffixRangeClass,

  -- * Conditionals
  ConditionalClass,
  RawConditional,

  -- * Checksums
  R2Checksums,
  R2ChecksumsClass,
  R2ChecksumsFields,

  -- * Re-exports
  R2,
  R2Class,
  module Servant.Cloudflare.Workers.R2,
) where

import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Servant.Cloudflare.Workers
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Blob (BlobClass)
import GHC.Wasm.Web.Generated.Headers (Headers)
import GHC.Wasm.Web.Generated.ReadableStream (ReadableStream)
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.R2 hiding (
  delete,
  deleteMany,
  get,
  getBody,
  getBodyArrayBuffer,
  getBodyBlob,
  getBodyText,
  getWith,
  head,
  isBodyUsed,
  list,
  list',
  put,
  putWith,
  writeObjectHttpMetadata,
 )
import Network.Cloudflare.Worker.Binding.R2 qualified as Raw
import Servant.Cloudflare.Workers.R2
import Prelude hiding (head)

head' ::
  forall es.
  (HasUniqueWorker es) =>
  R2 ->
  BS.ByteString ->
  Eff es (Promised (NullableClass R2ObjectClass) (Maybe R2Object))
head' r2 = unsafeEff_ . Raw.head r2

head ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  BS.ByteString ->
  Eff es (Promised (NullableClass R2ObjectClass) (Maybe R2Object))
head l = withBinding l . flip head'

get' ::
  forall es.
  (HasUniqueWorker es) =>
  R2 ->
  BS.ByteString ->
  Eff es (Promised (NullableClass R2ObjectBodyClass) (Maybe R2ObjectBody))
get' r2 = unsafeEff_ . Raw.get r2

get ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  BS.ByteString ->
  Eff es (Promised (NullableClass R2ObjectBodyClass) (Maybe R2ObjectBody))
get l = withBinding l . flip get'

getWith' ::
  forall es.
  (HasUniqueWorker es) =>
  R2 ->
  BS.ByteString ->
  RawGetOptions ->
  Eff es (Promised (NullableClass (UnionClass '[R2ObjectClass, R2ObjectBodyClass])) (Maybe (Either R2Object R2ObjectBody)))
getWith' r2 bs = unsafeEff_ . Raw.getWith r2 bs

getWith ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  BS.ByteString ->
  RawGetOptions ->
  Eff es (Promised (NullableClass (UnionClass '[R2ObjectClass, R2ObjectBodyClass])) (Maybe (Either R2Object R2ObjectBody)))
getWith l bs req = withBinding l \kv -> getWith' kv bs req

put' ::
  forall es.
  (HasUniqueWorker es) =>
  R2 ->
  BS.ByteString ->
  PutBody ->
  Eff es (Promised (NullableClass R2ObjectClass) (Maybe R2Object))
put' r2 = fmap unsafeEff_ . Raw.put r2

put ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  BS.ByteString ->
  PutBody ->
  Eff es (Promised (NullableClass R2ObjectClass) (Maybe R2Object))
put l bs bdy = withBinding l \r2 -> put' r2 bs bdy

putWith' ::
  forall es.
  (HasUniqueWorker es) =>
  R2 ->
  BS.ByteString ->
  PutBody ->
  RawPutOptions ->
  Eff es (Promised (NullableClass R2ObjectClass) (Maybe R2Object))
putWith' r2 = fmap (fmap unsafeEff_) . Raw.putWith r2

putWith ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  BS.ByteString ->
  PutBody ->
  RawPutOptions ->
  Eff es (Promised (NullableClass R2ObjectClass) (Maybe R2Object))
putWith l bs bdy opt = withBinding l \r2 -> putWith' r2 bs bdy opt

delete' ::
  forall es.
  (HasUniqueWorker es) =>
  R2 ->
  BS.ByteString ->
  Eff es (Promised UndefinedClass ())
delete' r2 = unsafeEff_ . Raw.delete r2

delete ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  BS.ByteString ->
  Eff es (Promised UndefinedClass ())
delete l = withBinding l . flip delete'

deleteMany' ::
  forall es.
  (HasUniqueWorker es) =>
  R2 ->
  V.Vector BS.ByteString ->
  Eff es (Promised UndefinedClass ())
deleteMany' r2 = unsafeEff_ . Raw.deleteMany r2

deleteMany ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  V.Vector BS.ByteString ->
  Eff es (Promised UndefinedClass ())
deleteMany l = withBinding l . flip deleteMany'

list' ::
  (HasUniqueWorker es) =>
  R2 ->
  Maybe RawListOptions ->
  Eff es (Promised R2ObjectsClass R2ObjectsView)
list' r2 = unsafeEff_ . Raw.list r2

list ::
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  Maybe RawListOptions ->
  Eff es (Promised R2ObjectsClass R2ObjectsView)
list l = withBinding l . flip list'

listRaw' ::
  (HasUniqueWorker es) =>
  R2 ->
  Nullable RawListOptionsClass ->
  Eff es (Promise R2ObjectsClass)
listRaw' r2 = unsafeEff_ . Raw.list' r2

listRaw ::
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ R2Class
  ) =>
  Nullable RawListOptionsClass ->
  Eff es (Promise R2ObjectsClass)
listRaw l = withBinding l . flip listRaw'

writeObjectHttpMetadata ::
  ( HasUniqueWorker es
  , r2Obj <: R2ObjectClass
  ) =>
  JSObject r2Obj ->
  Headers ->
  Eff es ()
writeObjectHttpMetadata = fmap unsafeEff_ . Raw.writeObjectHttpMetadata

getBody :: (HasUniqueWorker es) => R2ObjectBody -> Eff es ReadableStream
getBody = unsafeEff_ . Raw.getBody

isBodyUsed :: (HasUniqueWorker es) => R2ObjectBody -> Eff es Bool
isBodyUsed = unsafeEff_ . Raw.isBodyUsed

getBodyArrayBuffer :: (HasUniqueWorker es) => R2ObjectBody -> Eff es (Promise ArrayBufferClass)
getBodyArrayBuffer = unsafeEff_ . Raw.getBodyArrayBuffer

getBodyText :: (HasUniqueWorker es) => R2ObjectBody -> Eff es (Promise USVStringClass)
getBodyText = unsafeEff_ . Raw.getBodyText

getBodyBlob :: (HasUniqueWorker es) => R2ObjectBody -> Eff es (Promise BlobClass)
getBodyBlob = unsafeEff_ . Raw.getBodyBlob
