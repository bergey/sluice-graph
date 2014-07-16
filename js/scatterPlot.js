/* global React, d3, document, setInterval */

var scatterPlot = (function() {
    "use strict";

    var r = React.DOM;

    var testData = [[0,0], [2,5], [10,5], [15,20]];
    
    var plot = React.createClass({
        render: function() {
            return r.svg(
                { width: this.props.width,
                  height: this.props.height},
                _.map(testData, function(d,i) {return r.circle({
                    cx: 10*d[0],
                    cy: 10*d[1],
                    r: 2,
                    key: i,
                    fill: 'green'});}));
        }});

    return plot;
})()
