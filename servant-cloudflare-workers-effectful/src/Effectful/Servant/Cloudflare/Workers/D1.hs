{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Servant.Cloudflare.Workers.D1 (
  -- * Core Types
  D1,
  D1Class,
  Statement,
  StatementClass,
  D1ValueClass,
  D1Error (..),

  -- * Values and Rows
  D1Value,
  D1ValueAlts,
  D1ValueView (..),
  FromD1Value (..),
  ToD1Value (..),
  unviewD1Value,
  viewD1Value,
  D1Row,
  D1RowClass,
  D1RowView (..),
  ToD1Row (..),
  GenericToD1Row,
  genericToD1Row,
  genericToD1RowView,
  FromD1Row (..),
  GenericFromD1Row,
  genericFromD1Row,
  genericFromD1RowView,
  viewD1Row,
  unviewD1Row,

  -- * Prepared statements
  PreparedStatement,
  PreparedStatementClass,
  prepare,
  prepare',
  bind,
  bindRaw,

  -- * Queries

  -- ** Types for query resulsts
  D1Result,
  D1ResultClass,
  D1ResultFields,
  D1ResultView (..),
  D1MetadataClass,
  D1Metadata,
  D1MetadataView (..),

  -- ** Lists all rows
  all,
  allRaw,

  -- ** Lists all rows, but witout squashing columns
  raw,
  rawRaw,
  rawWithColumns,
  rawWithColumnsRaw,

  -- ** Fetches the first row or columns
  first,
  firstRaw,
  firstColumns,
  firstColumnsRaw,

  -- ** Runs a query without results
  run,
  runRaw,
  D1Metrics,
  D1MetricsFields,
  D1MetricsView (..),

  -- ** Batch run
  batch,
  batch',

  -- ** Executes a raw queries.
  exec,
  exec',
  execRaw,
  execRaw',
  D1ExecResult,
  D1ExecResultView (..),
  D1ExecResultClass,
  D1ExecResultFields,
) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Dispatch.Static
import Effectful.Servant.Cloudflare.Workers
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding.D1 hiding (all, all', batch, bind, bind', exec, exec', first, firstColumns, prepare, raw, raw', rawWithColumns, run)
import Network.Cloudflare.Worker.Binding.D1 qualified as Raw
import Prelude hiding (all)

prepare :: (HasUniqueWorker es) => D1 -> String -> Eff es PreparedStatement
prepare = fmap unsafeEff_ . Raw.prepare

prepare' ::
  forall l ->
  forall vs ss bs es.
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ D1Class
  ) =>
  String ->
  Eff es PreparedStatement
prepare' l = withBinding l . flip prepare

bind :: (HasUniqueWorker es) => PreparedStatement -> V.Vector D1ValueView -> Eff es Statement
bind = fmap unsafeEff_ . Raw.bind

bindRaw :: (HasUniqueWorker es) => PreparedStatement -> V.Vector D1Value -> Eff es Statement
bindRaw = fmap unsafeEff_ . Raw.bind'

all :: (HasUniqueWorker es) => Statement -> Eff es (Promised (D1ResultClass D1RowClass) (D1ResultView D1RowView))
all = unsafeEff_ . Raw.all

allRaw :: (HasUniqueWorker es) => Statement -> Eff es (Promise (D1ResultClass D1RowClass))
allRaw = unsafeEff_ . Raw.all'

raw :: (HasUniqueWorker es) => Statement -> Eff es (Promised (SequenceClass (SequenceClass D1ValueClass)) (V.Vector (V.Vector D1ValueView)))
raw = unsafeEff_ . Raw.raw

rawRaw :: (HasUniqueWorker es) => Statement -> Eff es (Promise (SequenceClass (SequenceClass D1ValueClass)))
rawRaw = unsafeEff_ . Raw.raw'

rawWithColumns ::
  (HasUniqueWorker es) =>
  Statement ->
  Eff es (Promised (SequenceClass (SequenceClass D1ValueClass)) (V.Vector T.Text, V.Vector (V.Vector D1ValueView)))
rawWithColumns = unsafeEff_ . Raw.rawWithColumns

rawWithColumnsRaw ::
  (HasUniqueWorker es) =>
  Statement ->
  Eff es (Promise (SequenceClass (SequenceClass D1ValueClass)))
rawWithColumnsRaw = unsafeEff_ . Raw.rawWithColumns'

first :: (HasUniqueWorker es) => Statement -> Eff es (Promised (NullableClass D1RowClass) (Maybe D1RowView))
first = unsafeEff_ . Raw.first

firstRaw :: (HasUniqueWorker es) => Statement -> Eff es (Promise (NullableClass D1RowClass))
firstRaw = unsafeEff_ . Raw.first'

firstColumns ::
  (HasUniqueWorker es) =>
  Statement ->
  V.Vector T.Text ->
  Eff es (Promised (NullableClass (SequenceClass D1ValueClass)) (Maybe (V.Vector D1ValueView)))
firstColumns = fmap unsafeEff_ . Raw.firstColumns

firstColumnsRaw ::
  (HasUniqueWorker es) =>
  Statement ->
  Sequence USVStringClass ->
  Eff es (Promise (NullableClass (SequenceClass D1ValueClass)))
firstColumnsRaw = fmap unsafeEff_ . Raw.firstColumns'

run :: (HasUniqueWorker es) => Statement -> Eff es (Promised D1MetricsClass D1MetricsView)
run = unsafeEff_ . Raw.run

runRaw :: (HasUniqueWorker es) => Statement -> Eff es (Promise D1MetricsClass)
runRaw = unsafeEff_ . Raw.run'

batch' ::
  (HasUniqueWorker es) =>
  D1 ->
  V.Vector Statement ->
  Eff
    es
    ( Promised
        (SequenceClass (D1ResultClass D1RowClass))
        (V.Vector (D1ResultView D1RowView))
    )
batch' d1 = unsafeEff_ . Raw.batch d1

batch ::
  forall l ->
  forall vs ss bs es.
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ D1Class
  ) =>
  V.Vector Statement ->
  Eff
    es
    ( Promised
        (SequenceClass (D1ResultClass D1RowClass))
        (V.Vector (D1ResultView D1RowView))
    )
batch l = withBinding l . flip batch'

exec' :: (HasUniqueWorker es) => D1 -> T.Text -> Eff es (Promised D1ExecResultClass D1ExecResultView)
exec' d1 = unsafeEff_ . Raw.exec d1

exec ::
  forall l ->
  forall vs ss bs es.
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ D1Class
  ) =>
  T.Text ->
  Eff es (Promised D1ExecResultClass D1ExecResultView)
exec l = withBinding l . flip exec'

execRaw' :: (HasUniqueWorker es) => D1 -> USVString -> Eff es (Promise D1ExecResultClass)
execRaw' d1 = unsafeEff_ . Raw.exec' d1

execRaw ::
  forall l ->
  forall vs ss bs es.
  ( HasUniqueWorkerWith (BindingsClass vs ss bs) es
  , Member l bs
  , Lookup' l bs ~ D1Class
  ) =>
  USVString ->
  Eff es (Promise D1ExecResultClass)
execRaw l = withBinding l . flip execRaw'
