module Ngrx.Reducers where

import Prelude

import Data.Array (any, foldl)
import Data.Foreign (isUndefined)
import Unsafe.Coerce (unsafeCoerce)

import Ngrx.Actions (Action, Matcher)


-- | Reducer type alias: t -> s
type Reducer p s = Action p -> s -> s

-- | Guard a reducer on action type.
caseFn :: ∀ p s. Matcher -> Reducer p s -> Reducer p s
caseFn matcher reducer action state
    | matcher action.type = reducer action state
    | otherwise           = state

-- | Guard a reducer on action types.
casesFn :: ∀ p s. Array Matcher -> Reducer p s -> Reducer p s
casesFn ms = caseFn $ \a -> any (\m -> m a) ms

-- | Compose caseFn reducers.
switchFn :: ∀ p s. Array (Reducer p s) -> Action p -> s -> s
switchFn rds a = foldl (\rs r -> rs <<< (r a)) id rds

-- | Init a reducer with initial state.
withInitialState :: ∀ p s. s -> Reducer p s -> Reducer p s
withInitialState is rd = \p s -> if isUndefined $ unsafeCoerce s then rd p is else rd p s
