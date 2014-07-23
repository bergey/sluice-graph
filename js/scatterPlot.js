/* global React, d3, document, setInterval */

var scatterPlot = (function() {
    "use strict";

    var r = React.DOM;

    var extent = function(data, key) {
        var a = d3.extent(data, key);
        return {
            min: a[0],
            max: a[1]
        };
    };

    var defaultScale = function(data, w) {
        return d3.scale.linear()
            .domain([data.min, data.max])
            .range([0, w]);
    };

    var translate = function(x, y) {
        return 'translate(' + x.toString() + ',' + y.toString() + ')';
    };

    var viewport = React.createClass({
        render: function() {
            var props = this.props;
            
            return r.g(
                { transform: props.transform},
                _.map(props.data, function(d,i) {
                    return r.circle({
                        cx: props.xScale(d),
                        cy: props.yScale(d),
                        r: 2,
                        key: i
                    });
                }));
        }});
    
    var plot = React.createClass({
        render: function() {
            var props = this.props; //  _ methods bind other *this*

            var viewWidth = props.width - props.margin.left - props.margin.right;
            var viewHeight = props.height - props.margin.top - props.margin.bottom;

            var cx = props.xScale(extent(props.data, props.xKey), viewWidth);
            var cy = props.yScale(extent(props.data, props.yKey), viewHeight);
                        
            return r.svg(
                { width: props.width,
                  height: props.height},
                viewport({
                    xScale: _.compose(cx, props.xKey),
                    yScale: _.compose(cy, props.yKey),
                    transform: translate(props.margin.left, props.margin.top),
                    data: props.data,
                    xKey: props.xKey,
                    yKey: props.yKey
                }));
        },

        getDefaultProps: function() {
                return {
                   width: 450,
                    height: 300,
                    xKey: _.property('x'),
                    yKey: _.property('y'),
                    // xScale and yScale are functions
                    // xScale :: (value summary, viewport dim) -> x value -> output coördinate
                    xScale: defaultScale,
                    yScale: defaultScale,
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
