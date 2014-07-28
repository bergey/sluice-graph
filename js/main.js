/* global document, setInterval */

define(['react', 'filterGraph'], function (React, filterGraph) {
    "use strict";

    var r = React.DOM;

    React.renderComponent(
        filterGraph(),
        document.getElementById('example'));
});
