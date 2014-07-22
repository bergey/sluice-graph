/* global React, d3, document, setInterval */

(function () {
    "use strict";

    var r = React.DOM;

    React.renderComponent(
            // scatterPlot({
            //     data: _.map([[0,0], [2,5], [10,5], [15,20]], function(d) {
            //         return {'x': d[0], 'y': d[1]};
            //     })}),
        filterGraph(),
        document.getElementById('example'));
})();
