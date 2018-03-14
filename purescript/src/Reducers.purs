module Ngrx.Reducers where

-- import Prelude
-- import Data.Array (foldr)

import Ngrx.Actions

type Reducer s m p r e = s -> Action m p r e -> s
type Handler s p r e = s -> Payload p r e -> s


caseFn :: String -> forall s p r e. Handler s p r e -> forall m. Reducer s m p r e
caseFn t h = \s a -> if a <?> t then h s a.payload else s


-- casesFn :: forall s p r e. Handler s p r e -> Array String -> forall m. Reducer s m p r e
-- casesFn h = foldr (\t acc -> \s a -> acc ((caseFn t h) s a) a) (\s a -> s)


-- reducerDefaultFn = true

-- reducerFn' :: forall s p p2 m. Reducer s p m -> Reducer s p2 m


-- reducerFn :: forall s p m. Array (Reducer s p m) -> Reducer s p m
-- reducerFn = foldr (\r acc -> \s a -> acc (r s a) a) (\s a -> a)
