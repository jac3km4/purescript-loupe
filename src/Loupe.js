var React = require('react');

exports.useReducer_ = function (reducer, initialState) {
    var self = React.useReducer(reducer, initialState);
    var state = self[0];
    var dispatch = self[1];
    return { state: state, dispatch: dispatch };
}

exports.useState_ = function (initialState) {
    var self = React.useState(initialState);
    var state = self[0];
    var setState = self[1];
    return { state: state, setState: setState };
}

exports.useEffect_ = function (fn, deps) {
    React.useEffect(fn, deps);
}


exports.useRef_ = function (initial) {
    var ref = React.useRef(initial);
    return { get: function () { return ref.current; }, set: function (v) { ref.current = v; } };
}