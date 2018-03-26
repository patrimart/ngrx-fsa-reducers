module Ngrx.Reducers where

import Prelude

import Data.Array (any, foldl)
import Data.Foreign (isUndefined)
import Unsafe.Coerce (unsafeCoerce)

import Ngrx.Actions (Action, Matcher)


-- | Reducer type alias: t -> s
type Reducer s = ∀ p. Action p -> s -> s

-- | Guard a reducer on action type.
caseFn :: ∀ s. Matcher -> Reducer s -> Reducer s
caseFn matcher reducer action state
    | matcher action = reducer action state
    | otherwise      = state

-- | Guard a reducer on action types.
casesFn :: ∀ s. Array Matcher -> Reducer s -> Reducer s
casesFn ms = caseFn $ \a -> any (\m -> m a) ms

-- | Compose caseFn reducers.
-- switchFn :: ∀ s. Array (Reducer s) -> Reducer s
-- switchFn rds = \a -> foldl (\rs r -> rs <<< r a) id rds

-- | Init a reducer with initial state.
withInitialState :: ∀ s. s -> Reducer s -> Reducer s
withInitialState is rd = \p s -> if isUndefined $ unsafeCoerce s then rd p is else rd p s
