{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Cloudflare.Workers.Internal.Router (
  CaptureHint (..),
  Router' (..),
  Router,
  runRouter,
  pathRouter,
  leafRouter,
  choice,
  routerLayout,
) where

import Data.Function (
  on,
 )
import Data.List (
  nub,
 )
import Data.Map (
  Map,
 )
import qualified Data.Map as M
import Data.Text (
  Text,
 )
import qualified Data.Text as T
import Data.Typeable (
  TypeRep,
 )
import Servant.Cloudflare.Workers.Internal.ErrorFormatter
import Servant.Cloudflare.Workers.Internal.RouteResult
import Servant.Cloudflare.Workers.Internal.RoutingApplication
import Servant.Cloudflare.Workers.Internal.ServerError

type Router b env = Router' env (RoutingApplication b)

-- | Holds information about pieces of url that are captured as variables.
data CaptureHint = CaptureHint
  { captureName :: Text
  -- ^ Holds the name of the captured variable
  , captureType :: TypeRep
  -- ^ Holds the type of the captured variable
  }
  deriving (Show, Eq)

toCaptureTag :: CaptureHint -> Text
toCaptureTag hint = captureName hint <> "::" <> (T.pack . show) (captureType hint)

toCaptureTags :: [CaptureHint] -> Text
toCaptureTags hints = "<" <> T.intercalate "|" (map toCaptureTag hints) <> ">"

{- | Internal representation of a router.

The first argument describes an environment type that is
expected as extra input by the routers at the leaves. The
environment is filled while running the router, with path
components that can be used to process captures.
-}
data Router' env a
  = -- | the map contains routers for subpaths (first path component used
    --   for lookup and removed afterwards), the list contains handlers
    --   for the empty path, to be tried in order
    StaticRouter (Map Text (Router' env a)) [env -> a]
  | -- | first path component is passed to the child router in its
    --   environment and removed afterwards.
    --   The first argument is a list of hints for all variables that can be
    --   captured by the router. The fact that it is a list is counter-intuitive,
    --   because the 'Capture' combinator only allows to capture a single varible,
    --   with a single name and a single type. However, the 'choice' smart
    --   constructor may merge two 'Capture' combinators with different hints, thus
    --   forcing the type to be '[CaptureHint]'.
    --   Because 'CaptureRouter' is built from a 'Capture' combinator, the list of
    --   hints should always be non-empty.
    CaptureRouter [CaptureHint] (Router' (Text, env) a)
  | -- | all path components are passed to the child router in its
    --   environment and are removed afterwards
    --   The first argument is a hint for the list of variables that can be
    --   captured by the router. Note that the 'captureType' field of the hint
    --   should always be '[a]' for some 'a'.
    CaptureAllRouter [CaptureHint] (Router' ([Text], env) a)
  | -- | to be used for routes we do not know anything about
    RawRouter (env -> a)
  | -- | left-biased choice between two routers
    Choice (Router' env a) (Router' env a)
  deriving (Functor)

-- | Smart constructor for a single static path component.
pathRouter :: Text -> Router' env a -> Router' env a
pathRouter t r = StaticRouter (M.singleton t r) []

{- | Smart constructor for a leaf, i.e., a router that expects
the empty path.
-}
leafRouter :: (env -> a) -> Router' env a
leafRouter l = StaticRouter M.empty [l]

{- | Smart constructor for the choice between routers.
We currently optimize the following cases:

  * Two static routers can be joined by joining their maps
    and concatenating their leaf-lists.
  * Two dynamic routers can be joined by joining their codomains.
  * Choice nodes can be reordered.
-}
choice :: Router' env a -> Router' env a -> Router' env a
choice (StaticRouter table1 ls1) (StaticRouter table2 ls2) =
  StaticRouter (M.unionWith choice table1 table2) (ls1 ++ ls2)
choice (CaptureRouter hints1 router1) (CaptureRouter hints2 router2) =
  CaptureRouter (nub $ hints1 ++ hints2) (choice router1 router2)
choice router1 (Choice router2 router3) = Choice (choice router1 router2) router3
choice router1 router2 = Choice router1 router2

{- | Datatype used for representing and debugging the
structure of a router. Abstracts from the handlers
at the leaves.

Two 'Router's can be structurally compared by computing
their 'RouterStructure' using 'routerStructure' and
then testing for equality, see 'sameStructure'.
-}
data RouterStructure
  = StaticRouterStructure (Map Text RouterStructure) Int
  | -- | The first argument holds information about variables
    -- that are captured by the router. There may be several hints
    -- if several routers have been aggregated by the 'choice'
    -- smart constructor.
    CaptureRouterStructure [CaptureHint] RouterStructure
  | RawRouterStructure
  | ChoiceStructure RouterStructure RouterStructure
  deriving (Eq, Show)

{- | Compute the structure of a router.

Assumes that the request or text being passed
in 'WithRequest' or 'CaptureRouter' does not
affect the structure of the underlying tree.
-}
routerStructure :: Router' env a -> RouterStructure
routerStructure (StaticRouter m ls) =
  StaticRouterStructure (fmap routerStructure m) (length ls)
routerStructure (CaptureRouter hints router) =
  CaptureRouterStructure hints $
    routerStructure router
routerStructure (CaptureAllRouter hints router) =
  CaptureRouterStructure hints $
    routerStructure router
routerStructure (RawRouter _) =
  RawRouterStructure
routerStructure (Choice r1 r2) =
  ChoiceStructure
    (routerStructure r1)
    (routerStructure r2)

{- | Provide a textual representation of the
structure of a router.
-}
routerLayout :: Router' env a -> Text
routerLayout router =
  T.unlines (["/"] ++ mkRouterLayout False (routerStructure router))
  where
    mkRouterLayout :: Bool -> RouterStructure -> [Text]
    mkRouterLayout c (StaticRouterStructure m n) = mkSubTrees c (M.toList m) n
    mkRouterLayout c (CaptureRouterStructure hints r) =
      mkSubTree c (toCaptureTags hints) (mkRouterLayout False r)
    mkRouterLayout c RawRouterStructure =
      if c then ["├─ <raw>"] else ["└─ <raw>"]
    mkRouterLayout c (ChoiceStructure r1 r2) =
      mkRouterLayout True r1 ++ ["┆"] ++ mkRouterLayout c r2

    mkSubTrees :: Bool -> [(Text, RouterStructure)] -> Int -> [Text]
    mkSubTrees _ [] 0 = []
    mkSubTrees c [] n =
      concat (replicate (n - 1) (mkLeaf True) ++ [mkLeaf c])
    mkSubTrees c [(t, r)] 0 =
      mkSubTree c t (mkRouterLayout False r)
    mkSubTrees c ((t, r) : trs) n =
      mkSubTree True t (mkRouterLayout False r) ++ mkSubTrees c trs n

    mkLeaf :: Bool -> [Text]
    mkLeaf True = ["├─•", "┆"]
    mkLeaf False = ["└─•"]

    mkSubTree :: Bool -> Text -> [Text] -> [Text]
    mkSubTree True path children = ("├─ " <> path <> "/") : map ("│  " <>) children
    mkSubTree False path children = ("└─ " <> path <> "/") : map ("   " <>) children

-- | Interpret a router as an application.
runRouter :: NotFoundErrorFormatter -> Router b () -> RoutingApplication b
runRouter fmt r = runRouterEnv fmt r ()

runRouterEnv :: NotFoundErrorFormatter -> Router b env -> env -> RoutingApplication b
runRouterEnv fmt router env request b ctx respond =
  case router of
    StaticRouter table ls ->
      case pathInfo request of
        [] -> runChoice fmt ls env request b ctx respond
        -- This case is to handle trailing slashes.
        [""] -> runChoice fmt ls env request b ctx respond
        first : rest
          | Just router' <- M.lookup first table ->
              let request' = request {pathInfo = rest}
               in runRouterEnv fmt router' env request' b ctx respond
        _ -> respond $ Fail $ fmt request.rawRequest
    CaptureRouter _ router' ->
      case pathInfo request of
        [] -> respond $ Fail $ fmt request.rawRequest
        -- This case is to handle trailing slashes.
        [""] -> respond $ Fail $ fmt request.rawRequest
        first : rest ->
          let request' = request {pathInfo = rest}
           in runRouterEnv fmt router' (first, env) request' b ctx respond
    CaptureAllRouter _ router' ->
      let segments = case pathInfo request of
            -- this case is to handle trailing slashes.
            ("" : xs) -> xs
            xs -> xs
          request' = request {pathInfo = []}
       in runRouterEnv fmt router' (segments, env) request' b ctx respond
    RawRouter app ->
      app env request b ctx respond
    Choice r1 r2 ->
      runChoice fmt [runRouterEnv fmt r1, runRouterEnv fmt r2] env request b ctx respond

{- | Try a list of routing applications in order.
We stop as soon as one fails fatally or succeeds.
If all fail normally, we pick the "best" error.
-}
runChoice :: NotFoundErrorFormatter -> [env -> RoutingApplication b] -> env -> RoutingApplication b
runChoice fmt ls =
  case ls of
    [] -> \_ request _ _ respond -> respond $ Fail $ fmt $ rawRequest request
    [r] -> r
    (r : rs) ->
      \env request b e respond ->
        r env request b e $ \response1 ->
          case response1 of
            Fail _ -> runChoice fmt rs env request b e $ \response2 ->
              respond $ highestPri response1 response2
            _ -> respond response1
  where
    highestPri (Fail e1) (Fail e2) =
      if worseHTTPCode (errHTTPCode e1) (errHTTPCode e2)
        then Fail e2
        else Fail e1
    highestPri (Fail _) y = y
    highestPri x _ = x

-- Priority on HTTP codes.
--
worseHTTPCode :: Int -> Int -> Bool
worseHTTPCode = on (<) toPriority
  where
    toPriority :: Int -> Int
    toPriority 404 = 0 -- not found
    toPriority 405 = 1 -- method not allowed
    toPriority 401 = 2 -- unauthorized
    toPriority 415 = 3 -- unsupported media type
    toPriority 406 = 4 -- not acceptable
    toPriority 400 = 6 -- bad request
    toPriority _ = 5
