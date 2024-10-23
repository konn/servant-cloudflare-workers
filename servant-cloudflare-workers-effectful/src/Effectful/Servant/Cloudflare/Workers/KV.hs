{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Servant.Cloudflare.Workers.KV (
  KV,
  KVClass,
  delete,
  delete',
  listKeys,
  listKeys',
  ListKeys (..),
  ListKeyResult (..),
  Key (..),
  get,
  get',
  getWithMetadata,
  getWithMetadata',
  ValueWithMetadata (..),
  put,
  put',
  PutOptions (..),
  Cursor (..),
) where

import Effectful.Dispatch.Static
import Effectful.Servant.Cloudflare.Workers
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.KV hiding (delete, get, getWithMetadata, listKeys, put)
import Network.Cloudflare.Worker.Binding.KV qualified as Raw

delete' :: (HasUniqueWorker es) => KV -> String -> Eff es ()
delete' kv key = unsafeEff_ $ Raw.delete kv key

delete ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ KVClass
  ) =>
  String ->
  Eff es ()
delete l = withBinding l . flip delete'

listKeys' :: (HasUniqueWorker es) => KV -> ListKeys -> Eff es (Either String ListKeyResult)
listKeys' kv = unsafeEff_ . Raw.listKeys kv

listKeys ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ KVClass
  ) =>
  ListKeys ->
  Eff es (Either String ListKeyResult)
listKeys l = withBinding l . flip listKeys'

get' :: (HasUniqueWorker es) => KV -> String -> Eff es (Maybe String)
get' = fmap unsafeEff_ . Raw.get

get ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ KVClass
  ) =>
  String ->
  Eff es (Maybe String)
get l = withBinding l . flip get'

getWithMetadata' :: (HasUniqueWorker es) => KV -> String -> Eff es (Maybe ValueWithMetadata)
getWithMetadata' = fmap unsafeEff_ . Raw.getWithMetadata

getWithMetadata ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ KVClass
  ) =>
  String ->
  Eff es (Maybe ValueWithMetadata)
getWithMetadata l = withBinding l . flip getWithMetadata'

put' :: (HasUniqueWorker es) => KV -> PutOptions -> String -> String -> Eff es ()
put' kv po k v = unsafeEff_ $ Raw.put kv po k v

put ::
  forall vs ss bs es.
  forall l ->
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ KVClass
  ) =>
  PutOptions ->
  String ->
  String ->
  Eff es ()
put l po k v = withBinding l $ \kv -> put' kv po k v
