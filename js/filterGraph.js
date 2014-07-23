/* global React, document, setInterval */

var filterGraph = (function() {
    "use strict";

    var r = React.DOM;

    var filterBldgs = function(criteria) {
        return function(d) {
            return criteria.minSF < d.BLDG_FLOOR_AREA
                && d.BLDG_FLOOR_AREA < criteria.maxSF
                && criteria.minEui < d.SITE_EUI
                && d.SITE_EUI < criteria.maxEui;
        };
    }

    var ret = React.createClass({
        render: function() {
            return r.div({},
                         // graph, fed data as props with filter
                         scatterPlot({
                             data: _.filter(this.state.data, filterBldgs(this.state)),
                             xKey: _.property('BLDG_FLOOR_AREA'),
                             yKey: _.property('SITE_EUI')
                         }),
                         filterForm({
                             minSF: this.state.minSF,
                             maxSF: this.state.maxSF,
                             minEui: this.state.minEui,
                             maxEui: this.state.maxEui,
                             setMinSF: this.setMinSF,
                             setMaxSF: this.setMaxSF,
                             setMinEui: this.setMinEui,
                             setMaxEui: this.setMaxEui
                         }));
        },
        
        componentDidMount: function() {
            // load the JSON data into State
            d3.csv("Benchmarking_Data_Public.csv", function(error, json) {
                var numeric = function(d) {
                    _.forEach(['ASSET_ID', 'BLDG_FLOOR_AREA', 'SITE_EUI'], function(key) {
                        d[key] = parseFloat(d[key]);
                    });
                    return d;
                };
                
                if (error) return console.warn(error);
                this.setState({data: _.map(json, numeric)});
            }.bind(this));
        },

    getInitialState: function() {
        return {
            minSF: 0,
            maxSF: 20000,
            minEui: 0,
            maxEui: 3000,
            data: _.map([[0,0], [2,5], [10,5], [15,20]], function(d) {
                return {'x': d[0], 'y': d[1]};
            })}},

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

    })

    return ret;
})()
