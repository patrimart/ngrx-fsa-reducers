module Ngrx.Actions
    ( ActionCreator
    , Action
    , Payload
    , Success
    , Failure
    , createAction
    , createAsyncAction
    , isType
    ) where

import Prelude
import Data.String (length, take)


newtype Payload p = Payload p

type Success p r =
    { params :: Payload p
    , result :: r }

type Failure p e =
    { params :: Payload p
    , error  :: e }

type Action p m =
    { type    :: String
    , payload :: Payload p
    , meta    :: m
    , error   :: Boolean }

type ActionCreator p = forall m. p -> m -> Action p m

type AsyncActionCreator p r e =
    { start  :: ActionCreator p
    , done   :: ActionCreator (Success p r)
    , failed :: ActionCreator (Failure p e) }


isType :: forall p m. String -> Action p m -> Boolean
isType t a = a.type == take (length a.type) t


createAction :: forall p. String -> ActionCreator p
createAction t = \p m -> { type: t, payload: Payload p, meta: m, error: false }


createAsyncAction :: forall p r e. String -> AsyncActionCreator p r e
createAsyncAction t =
    { start: createAction $ t <> "::Start"
    , done: createAction $ t <> "::Done"
    , failed: createAction $ t <> "::Failed" }
