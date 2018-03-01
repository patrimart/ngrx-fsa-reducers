# @ngrx-fsa/reducers

## A reducers generator with functional composition for @ngrx.

### Install

```
npm i @ngrx-fsa/reducers
```

### Usage

Example:

```ts
import { Action, ActionCreator, isType } from "typescript-fsa";
import { caseFn, reducerDefaultFn } from "@ngrx-fsa/reducers";

const actionCreator = actionCreatorFactory();

interface State {
    data: string;
}

const initialState: State = { data: "hello" };

const sliceData = actionCreator<number>("SLICE_DATA");
function sliceDataHandler(state: State, fromIndex: number): State {
    return { data: state.data.slice(fromIndex) };
}

const dataToUpperCase = actionCreator<void>("DATA_TO_UPPERCASE");
function dataToUpperCaseHandler(state: State): State {
    return { data: state.data.toUpperCase() };
}

const reducer = reducerDefaultFn(
    caseFn(sliceData, sliceDataHandler),
    caseFn(dataToUpperCase, dataToUpperCaseHandler),
)(initialState);
```
