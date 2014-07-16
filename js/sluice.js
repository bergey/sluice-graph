/* global React, d3, document, setInterval */

(function () {
    "use strict";

    var r = React.DOM;

    setInterval(function() {
        React.renderComponent(
            scatterPlot(),
            document.getElementById('example'));
    }, 500);
})();
