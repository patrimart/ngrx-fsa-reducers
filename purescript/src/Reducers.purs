module Ngrx.Reducers where

import Prelude
import Data.Array (foldr)

import Ngrx.Actions (Action, ActionCreator, Payload, isType)

type Reducer s p m = s -> Action p m -> s
type Handler s p = s -> Payload p -> s



caseFn :: forall s p m. String -> Handler s p -> Reducer s p m
caseFn t h = \s a -> if isType t a then h s a.payload else s


casesFn :: forall s p m. Handler s p -> Array String -> Reducer s p m
casesFn h = foldr (\acc ac -> \s a -> acc (caseFn ac h) s a) id


reducerDefaultFn = true


reducerFn = true
