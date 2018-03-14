module Ngrx.Actions where

import Prelude

import Data.String (length, take)

type Success p r =
    { params :: p
    , result :: r }

type Failure p e =
    { params :: p
    , error  :: e }

data Payload p r e = None | Do p | Success p r | Failure p e

type Action m p r e =
    { type    :: String
    , meta    :: m
    , payload :: Payload p r e
    , error   :: Boolean }

type ActionCreator p r e = Payload p r e -> Action Unit p r e

type AsyncActionCreator p r e = String ->
    { start  :: ActionCreator p Unit Unit
    , done   :: ActionCreator p r Unit
    , failed :: ActionCreator p Unit e }


isType :: forall m p r e. Action m p r e -> String -> Boolean
isType a t = a.type == take (length a.type) t

infixr 1 isType as <?>


createAction :: String -> forall m. m -> forall p r e. Payload p r e -> Action m p r e
createAction t meta (Failure p e) = { type: t, meta, payload: Failure p e, error: true }
createAction t meta payload       = { type: t, meta, payload, error: false }


withMeta :: forall p r e. Action Unit p r e -> forall m. m -> Action m p r e
withMeta a m = createAction a.type m a.payload

infixr 1 withMeta as >!<


createActionFactory :: String ->
    { empty :: String -> Action Unit Unit Unit Unit
    , sync  :: String -> forall p. ActionCreator p Unit Unit
    , async :: forall p r e. AsyncActionCreator p r e }
createActionFactory t =
    { empty : \t2   -> createAction (t <> "::" <> t2) unit None
    , sync  : \t2 p -> createAction (t <> "::" <> t2) unit p
    , async : \t2 ->
        { start : \p -> createAction (t <> "::" <> t2 <> "::Start") unit p
        , done  : \p -> createAction (t <> "::" <> t2 <> "::Done") unit p
        , failed: \p -> createAction (t <> "::" <> t2 <> "::Failed") unit p } }
