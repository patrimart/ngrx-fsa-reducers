module Ngrx.Actions where

import Prelude
import Data.String (length, take)

type Start p = p

newtype Success p r = Success
    { params :: p
    , result :: r }

type Failure p e =
    { params :: p
    , error  :: e }

type Action m pl =
    { type    :: String
    , meta    :: m
    , payload :: pl
    , error   :: Boolean }

data Payload p r e = None | Do p | Done p r | Failed p e

-- type ActionCreator p r e = Payload p r e -> Action Unit (Payload p r e)

-- type AsyncActionCreator p r e = String ->
--     { start  :: ActionCreator p Unit Unit
--     , done   :: ActionCreator p r Unit
--     , failed :: ActionCreator p Unit e }

-- | Tests if an Action is of a type.
-- isType :: { "type" :: String } -> String -> Boolean
isType :: forall m p. Action m p -> String -> Boolean
isType a t = a.type == take (length a.type) t


class CreateAction m p r e pl where
    createAction :: String -> m -> Payload p r e -> Action m pl

instance createActionEmpty :: CreateAction m Unit Unit Unit Unit where
    createAction t m None = { type: t, meta: m, error: false, payload: unit }

instance createActionStart :: CreateAction m p Unit Unit p where
    createAction t m (Do p) = { type: t, meta: m, error: false, payload: p }
    -- createAction _ _ _      = throw $ Error ""

-- | Creates an Action.
-- createAction :: String -> forall m. m -> forall p r e pl. Payload p r e -> Action m pl
-- createAction t meta None         = { type: t, meta, error: false, payload: unit }
-- createAction t meta (Do p)       = { type: t, meta, error: false, payload: p }
-- createAction t meta (Done p r)   = { type: t, meta, error: false, payload: { params: p, result: r } }
-- createAction t meta (Failed p e) = { type: t, meta, error: true,  payload: { params: p, error: e } }

-- | Add metadata to an Action.
-- withMeta :: forall p. Action Unit p -> forall m. m -> Action m p
-- withMeta a m = createAction a.type m a.payload


createEmptyAction :: String -> Action Unit Unit
createEmptyAction t = { type: t, meta: unit, error: false, payload: unit }

createStartAction :: String -> forall p. p -> Action Unit (Start p)
createStartAction t p = { type: t, meta: unit, error: false, payload: p }

createDoneAction :: String -> forall p. p -> forall r. r -> Action Unit (Success p r)
createDoneAction t p r = { type: t, meta: unit, error: false, payload: { params: p, result: r } }

createFailedAction :: String -> forall p. p -> forall e. e -> Action Unit (Failure p e)
createFailedAction t p e = { type: t, meta: unit, error: false, payload: { params: p, error: e } }


-- createActionFactory t =
--     { empty : \t2   -> createEmptyAction (t <> "::" <> t2)
--     , sync  : \t2 p -> createSyncAction (t <> "::" <> t2) unit p
--     , async : \t2 ->
--         { start : createStartAction (t <> "::" <> t2 <> "::Start") unit
--         , done  : createDoneAction (t <> "::" <> t2 <> "::Done") unit
--         , failed: createFailedAction (t <> "::" <> t2 <> "::Failed") unit } }
