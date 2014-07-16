"use strict;"

var r = React.DOM;

var HelloWorld = React.createClass({
    render: function() {
        return r.div(
            {},
            r.p({}, 'Hello, ', this.props.name, '!'),
            r.p({}, 'It is ', this.props.date.toTimeString()));
    }});

setInterval(function() {
    React.renderComponent(
        HelloWorld(
            {'name': 'Daniel',
             'date': new Date()}),
            document.getElementById('example'))
}, 500);
