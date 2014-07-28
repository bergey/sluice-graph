/* global React, document, setInterval */

var filterForm = (function() {
    "use strict";

    var r = React.DOM;

    var numRange = React.createClass({
        displayName: 'numRange',
        render: function() {
            return r.div({},
                         this.props.text + " between ",
                         r.input({
                             type: "number",
                             value: this.props.min,
                             onChange: this.props.minChange
                         }),
                         "and",
                         r.input({
                             type: "number",
                             value: this.props.max,
                             onChange: this.props.maxChange
                         }),
                         this.props.unit);
        }
    });

    var form = React.createClass({
        displayName: 'filterForm',
        render: function() {
            return r.div({},
                         numRange({
                             text: "Floor area",
                             unit: "square feet",
                             min: this.props.minSF,
                             max: this.props.maxSF,
                             minChange: this.props.setMinSF,
                             maxChange: this.props.setMaxSF,
                         }),
                         numRange({
                             text: "Energy Use Intensity",
                             unit: "kBTU/sf/yr",
                             min: this.props.minEui,
                             max: this.props.maxEui,
                             minChange: this.props.setMinEui,
                             maxChange: this.props.setMaxEui
                         }));
        },

    });
    
    return form;
})()
