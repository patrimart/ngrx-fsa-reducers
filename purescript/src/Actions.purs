module Ngrx.Actions
    ( Empty
    , Start
    , Success
    , Failure
    , Meta
    , Action
    , Matcher
    , withMeta
    , createActionFactory
) where

import Prelude

import Data.Record.Builder (build, merge)
import Data.String (joinWith)


test :: forall a. a -> a
test = id <<< id <<< id


-- | A payload of Unit value.
type Empty = ( payload :: Unit )

-- | A simple payload with a value.
type Start p = ( payload :: p )

-- | A Success payload with params and result.
type Success p r =
    ( payload ::
        { params :: p
        , result :: r } )

-- | A Failure payload with params and error.
type Failure p e =
    ( payload ::
        { params :: p
        , error  :: e } )

-- | A Meta record with payload.
type Meta m pl = ( meta :: m | pl )

-- | A basic FSA Action.
type Action payload =
    { type    :: String
    , error   :: Boolean
    | payload }

-- | Matcher type alias.
type Matcher = String -> Boolean

-- | Add metadata to an Action.
withMeta :: ∀ m. m -> ∀ p. Action p -> Action (Meta m p)
withMeta m a = build (merge a) { meta: m }

-- | An Action creator factory.
createActionFactory :: String ->
    { empty :: String -> { match :: Matcher, build :: Action Empty }
    , start :: String -> { match :: Matcher, build :: ∀ p. p -> Action (Start p) }
    , async :: String -> ∀ p r e.
        { empty  :: { match :: Matcher, build :: Action Empty }
        , start  :: { match :: Matcher, build :: p -> Action (Start p) }
        , done   :: { match :: Matcher, build :: p -> r -> Action (Success p r) }
        , failed :: { match :: Matcher, build :: p -> e -> Action (Failure p e) } } }
createActionFactory t =
    { empty : \t2 -> let tt = j [t, t2] in { match: ofType tt, build: createEmptyAction tt }
    , start : \t2 -> let tt = j [t, t2] in { match: ofType tt, build: createStartAction tt }
    , async : \t2 -> let tt = j [t, t2] in
        { empty : let ttt = j [tt, "EMPTY"]  in { match: ofType ttt, build: createEmptyAction tt }
        , start : let ttt = j [tt, "START"]  in { match: ofType ttt, build: createStartAction tt }
        , done  : let ttt = j [tt, "DONE"]   in { match: ofType ttt, build: createDoneAction tt }
        , failed: let ttt = j [tt, "FAILED"] in { match: ofType ttt, build: createFailedAction tt } } }


-- | --------------------------------------------------------------------------
-- | Private functions below

j :: Array String -> String
j = joinWith "__"

-- | Tests if an Action is of (type :: String)
ofType :: String -> Matcher
ofType t a = t == a

createEmptyAction :: String -> Action Empty
createEmptyAction t = { type: t, error: false, payload: unit }

createStartAction :: String -> ∀ p. p -> Action (Start p)
createStartAction t p = { type: t, error: false, payload: p }

createDoneAction :: String -> ∀ p. p -> ∀ r. r -> Action (Success p r)
createDoneAction t p r = { type: t, error: false, payload: { params: p, result: r } }

createFailedAction :: String -> ∀ p. p -> ∀ e. e -> Action (Failure p e)
createFailedAction t p e = { type: t, error: true, payload: { params: p, error: e } }
