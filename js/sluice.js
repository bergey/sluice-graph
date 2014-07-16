"use strict;"

var r = React.DOM;


var LikeButton = React.createClass({
    getInitialState: _.constant( {liked: false} ),
    handleClick: function(event) {
        this.setState( {liked: !this.state.liked} )
    },
    render: function() {
        var text = this.state.liked ? 'like' : 'unlike';
        return(r.p({onClick: this.handleClick}, 'You ', text, ' this.  Click to toggle.'))
    }
});

var HelloWorld = React.createClass({
    render: function() {
        return r.div(
            {},
            r.p({}, 'Hello, ', this.props.name, '!'),
            r.p({}, 'It is ', this.props.date.toTimeString()),
            LikeButton()
        );
    }});

setInterval(function() {
    React.renderComponent(
        HelloWorld(
            {'name': 'Daniel',
             'date': new Date()}),
            document.getElementById('example'))
}, 500);
