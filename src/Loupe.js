var React = require('react');

exports.useReducer_ = function (reducer, initialState) {
    var self = React.useReducer(reducer, initialState);
    var state = self[0];
    var dispatch = self[1];
    return { state: state, dispatch: dispatch };
}
