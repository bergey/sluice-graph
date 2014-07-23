/* global React, d3, document, setInterval */

var scatterPlot = (function() {
    "use strict";

    var r = React.DOM;

    var plot = React.createClass({
        render: function() {
            var props = this.props; //  _ methods bind other *this*

            // if props is not set, default scale is linear,
            // fits range of data to the available space (without padding)
            var cx, cy;
            if (props.xScale !== null) {
                cx = props.xScale;
            } else {
                cx = d3.scale.linear()
                    .domain(d3.extent(props.data, props.xKey))
                    .range([0, props.width - props.margin.left - props.margin.right]);
            }
            if (props.yScale !== null) {
                cy = props.yScale;
            } else {
                cy = d3.scale.linear()
                    .domain(d3.extent(props.data, props.yKey))
                    .range([0, props.height - props.margin.top - props.margin.bottom]);
            }
                        
            return r.svg(
                { width: props.width,
                  height: props.height},
                _.map(props.data, function(d,i) {
                    return r.circle({
                        cx: cx(props.xKey(d)),
                        cy: cy(props.yKey(d)),
                        r: 2,
                        key: i
                    });
                }));
        },

            getDefaultProps: function() {
                return {
                   width: 450,
                    height: 300,
                    xKey: _.property('x'),
                    yKey: _.property('y'),
                    xScale: null,
                    yScale: null,
                    margin: {
                        left: 20,
                        right: 20,
                        top: 20,
                        bottom: 20
                    },
                    data: []
                };
            }});

    return plot;
})()
