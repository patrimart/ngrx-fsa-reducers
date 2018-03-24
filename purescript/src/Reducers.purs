module Ngrx.Reducers where

import Prelude (id, otherwise, ($))
import Data.Tuple (Tuple(..))
import Data.Array (any)

import Ngrx.Actions (Action)

type State s = s
type Handler s p = State s -> Action p -> State s
type Reducer s p = Tuple (Action p) (State s) -> State s

-- |
caseFn :: ∀ mp. (Action mp -> Boolean) -> ∀ s. Handler s mp -> Reducer s mp
caseFn matcher reducer (Tuple action state)
    | matcher action = reducer state action
    | otherwise      = state


casesFn :: ∀ mp. Array (Action mp -> Boolean) -> ∀ s. Handler s mp -> Reducer s mp
casesFn matchers = caseFn $ any id matchers


switchFn :: ∀ mp s. Reducer s mp -> ∀ mp2 s. Reducer s mp2
switchFn c = switchFn c
