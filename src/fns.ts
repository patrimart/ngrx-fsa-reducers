import { Action, ActionCreator, isType } from "typescript-fsa";

import { CasesFn, Handler, Reducer, ReducerDefaultFn, ReducerFn } from "./interfaces";

/**
 * Case function matches ACtionCreator to handler.
 */
export const caseFn =
    <S, P>(
        actionCreator: ActionCreator<P>,
        handler: Handler<S, P>,
    ): Reducer<S, P> =>
        (s: S, a: Action<P>) => isType(a, actionCreator) ? handler(s, a.payload) : s;

/**
 * Case function matches multiple ActionCreators to a handler.
 */
export const casesFn: CasesFn =
    <S>(
        actionCreators: Array<ActionCreator<any>>,
        handler: Handler<S, any>,
    ): Reducer<S, any> =>
        actionCreators.reduceRight(
            (ra, ac) => (s: S, a: any) => ra(caseFn(ac, handler)(s, a), a),
            (s: S, _: any) => s,
        );

/**
 * Compose caseFn and casesFn with initial state.
 */
export const reducerDefaultFn: ReducerDefaultFn =
    <S>(...cases: Array<Reducer<S, any>>) =>
    (initialState: S): Reducer<S, any> =>
        (s = initialState, a: any) => (reducerFn as any)(...cases)(s, a);

/**
 * Compose caseFn and casesFn.
 */
export const reducerFn: ReducerFn =
    <S>(...cases: Array<Reducer<S, any>>): Reducer<S, any> =>
        (state: S, action: any) =>
            cases.reduceRight(
                (ra, r) => (s: S, a: any) => ra(r(s, a), a),
                (s: S, _: any) => s,
            )(state, action);
