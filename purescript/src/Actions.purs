module Ngrx.Actions
    ( Empty
    , Start
    , Success
    , Failure
    , Meta
    , Action
    , withMeta
    , createActionFactory
) where

import Prelude
-- import Data.String (length, take)

type Empty = ( payload :: Unit )

type Start p = ( payload :: p )

type Success p r =
    ( payload ::
        { params :: p
        , result :: r } )

type Failure p e =
    ( payload ::
        { params :: p
        , error  :: e } )

type Meta m pl =
    ( meta :: m
    | pl )

type Action payload =
    { type    :: String
    , error   :: Boolean
    | payload }


-- data Payload p r e = None | Do p | Done p r | Failed p e

-- | Tests if an Action is of a type.
-- isType :: ∀ mp. Action mp -> String -> Boolean
-- isType a t = a.type == take (length a.type) t

-- | Add metadata to an Action.
withMeta :: ∀ m. m -> ∀ p. Action ( payload :: p) -> Action (payload :: p, meta :: m)
withMeta m a = { type: a.type, error: a.error, meta: m, payload: a.payload }

match :: String -> ∀ mp. Action mp -> Boolean
match t a = t == a.type


createEmptyAction :: String -> Action Empty
createEmptyAction t = { type: t, error: false, payload: unit }

createStartAction :: String -> ∀ p. p -> Action (Start p)
createStartAction t p = { type: t, error: false, payload: p }

createDoneAction :: String -> ∀ p. p -> ∀ r. r -> Action (Success p r)
createDoneAction t p r = { type: t, error: false, payload: { params: p, result: r } }

createFailedAction :: String -> ∀ p. p -> ∀ e. e -> Action (Failure p e)
createFailedAction t p e = { type: t, error: true, payload: { params: p, error: e } }


createActionFactory :: String ->
    { empty :: String -> { match :: ∀ mp. Action mp -> Boolean, build :: Action Empty }
    , start :: String -> { match :: ∀ mp. Action mp -> Boolean, build :: ∀ p. p -> Action (Start p) }
    , async :: String -> ∀ p r e.
        { empty  :: { match :: ∀ mp. Action mp -> Boolean, build :: Action Empty }
        , start  :: { match :: ∀ mp. Action mp -> Boolean, build :: p -> Action (Start p) }
        , done   :: { match :: ∀ mp. Action mp -> Boolean, build :: p -> r -> Action (Success p r) }
        , failed :: { match :: ∀ mp. Action mp -> Boolean, build :: p -> e -> Action (Failure p e) } } }
createActionFactory t =
    { empty : \t2 -> let tt = (t <> "::" <> t2) in { match: match tt, build: createEmptyAction tt }
    , start : \t2 -> let tt = (t <> "::" <> t2) in { match: match tt, build: createStartAction tt }
    , async : \t2 ->
        { empty : let tt = (t <> "::" <> t2 <> "::EMPTY")  in { match: match tt, build: createEmptyAction tt }
        , start : let tt = (t <> "::" <> t2 <> "::START")  in { match: match tt, build: createStartAction tt }
        , done  : let tt = (t <> "::" <> t2 <> "::DONE")   in { match: match tt, build: createDoneAction tt }
        , failed: let tt = (t <> "::" <> t2 <> "::FAILED") in { match: match tt, build: createFailedAction tt } } }
