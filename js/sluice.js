/* global React, d3, document, setInterval */

(function () {
    "use strict";

    var r = React.DOM;

    var svgTest = React.createClass({
        render: function() {
            return r.svg(
                {},
                r.path({
                    d: d3.svg.line()([[10,10],[30,10], [30,30], [10,30], [10,10]])
                }),
                r.circle({
                    cx: 20,
                    cy: 20,
                    r: 10,
                    fill: 'blue'})
            );}});

    setInterval(function() {
        React.renderComponent(
            svgTest(),
            document.getElementById('example'));
    }, 500);
})();
