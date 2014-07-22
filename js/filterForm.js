/* global React, document, setInterval */

var filterForm = (function() {
    "use strict";

    var r = React.DOM;

    var numRange = React.createClass({
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
        render: function() {
            return r.div({},
                         numRange({
                             text: "Floor area",
                             unit: "square feet",
                             min: this.state.minSF,
                             max: this.state.maxSF,
                             minChange: this.setMinSF,
                             maxChange: this.setMaxSF,
                         }),
                         numRange({
                             text: "Energy Use Intensity",
                             unit: "kBTU/sf/yr",
                             min: this.state.minEui,
                             max: this.state.maxEui,
                             minChange: this.setMinEui,
                             maxChange: this.setMaxEui
                         }));
        },

        getInitialState: function() {
            return {
                minSF: 0,
                maxSF: 20000,
                minEui: 0,
                maxEui: 3000
            }
        },

        setMinSF: function(event) {
            this.setState({minSF: parseInt(event.target.value)});
        },
        setMaxSF: function(event) {
            this.setState({maxSF: parseInt(event.target.value)});
        },
        setMinEui: function(event) {
            this.setState({minEui: parseInt(event.target.value)});
        },
        setMaxEui: function(event) {
            this.setState({maxEui: parseInt(event.target.value)});
        }
    });
    
    return form;
})()
