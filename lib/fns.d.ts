import { ActionCreator } from "typescript-fsa";
import { CasesFn, Handler, Reducer, ReducerDefaultFn, ReducerFn } from "./interfaces";
export declare const caseFn: <S, P>(actionCreator: ActionCreator<P>, handler: Handler<S, P>) => Reducer<S, P>;
export declare const casesFn: CasesFn;
export declare const reducerDefaultFn: ReducerDefaultFn;
export declare const reducerFn: ReducerFn;
