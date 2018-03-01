"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var typescript_fsa_1 = require("typescript-fsa");
exports.caseFn = function (actionCreator, handler) {
    return function (s, a) { return typescript_fsa_1.isType(a, actionCreator) ? handler(s, a.payload) : s; };
};
exports.casesFn = function (actionCreators, handler) {
    return actionCreators.reduceRight(function (ra, ac) { return function (s, a) { return ra(exports.caseFn(ac, handler)(s, a), a); }; }, function (s, _) { return s; });
};
exports.reducerDefaultFn = function () {
    var cases = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        cases[_i] = arguments[_i];
    }
    return function (initialState) {
        return function (s, a) {
            if (s === void 0) { s = initialState; }
            return exports.reducerFn.apply(void 0, cases)(s, a);
        };
    };
};
exports.reducerFn = function () {
    var cases = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        cases[_i] = arguments[_i];
    }
    return function (state, action) {
        return cases.reduceRight(function (ra, r) { return function (s, a) { return ra(r(s, a), a); }; }, function (s, _) { return s; })(state, action);
    };
};
//# sourceMappingURL=fns.js.map