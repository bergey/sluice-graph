/* global document, setInterval */

define(['react', 'd3', 'underscore'], function(React, d3, _) {
    "use strict";

    var r = React.DOM;

    var extent = function(data, key) {
        if (data.length < 1) {
                return {min: 0, max: 0};
            } else {
                var a = d3.extent(data, key);
                return {
                    min: a[0],
                    max: a[1]
                };
            }
    };

    var translate = function(x, y) {
        return 'translate(' + x.toString() + ',' + y.toString() + ')';
    };

    // viewport expects these props:
    // transform: a string in SVG transform format, relative to the parent
    // data: an array of data, in any format
    // xScale: a function from a single datum to a number, in SVG units
    // yScale: a function from a single datum to a number, in SVG units
    var viewport = React.createClass({
        displayName: 'scatterViewport',
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

    // TODO better linspace function
    var linspace = function(min, max, count) {
        var step = (max - min) / count;
        return _.range(min, max, step);
    };

    // tickMark expects these props:
    // scale: a function mapping the domain to SVG pixels : Num -> Num
    // orient: one of 'top', 'bottom', 'right', 'left' : String
    // tickLength: Num
    // tickLabelSize: Num
    // 
    var tickMark = function(orient, scale, tickLength, labelSize) {
        switch (orient) {
           case "bottom":
            {
                return function(t) {
                    return r.line({
                        x1: scale(t),
                        y1: 0,
                        x2: scale(t),
                        y2: tickLength,
                        key: t    
                    });
                };
            }
            case "top":
            {
                return function(t) {
                    return r.line({
                        x1: scale(t),
                        y1: 0,
                        x2: scale(t),
                        y2: -tickLength,
                        key: t
                    });
                };
            }
            case "left":
            {
            return function(t) {
                return r.line({
                    x1: 0,
                    y1: scale(t),
                    x2: -tickLength,
                    y2: scale(t),
                    key: t
                });
            };
            }
            case "right":
            {
                return function(t) {
                    return r.line({
                        x1: 0,
                        y1: scale(t),
                        x2: tickLength,
                        y2: scale(t),
                        key: t
                    });
                };
            }
        }};

    var tickText = function(orient, scale, tickLength, labelSize) {
        var style;
        var pos = tickLength + labelSize
        switch (orient) {
            case "bottom":
            {
                style = {"font-size": labelSize + "px;", "text-anchor": "center"};
                return function(t) {
                    return r.text({
                        x: scale(t),
                        y: pos,
                        style: style
                    }, t.toString());
                };
            }
            case "top":
            {
                style = {"font-size": labelSize + "px;", "text-anchor": "center"};
                return function(t) {
                    return r.text({x: scale(t), y: -pos, style: style}, t.toString());
                };
            }
            case "left":
            {
                style = {"font-size": labelSize + "px;", "text-anchor": "end"};
                return function(t) {
                    return r.text({x: -tickLength, y: scale(t) + labelSize/2, style: style}, t.toString());
                };
            }
            case "right":
            {
                style = {"font-size": labelSize + "px;", "text-anchor": "start"};
                return function(t) {
                    return r.text({x: tickLength, y: scale(t) + labelSize/2, style: style}, t.toString());
                };
            }
        }};

    var axisLine = function(orient, scale, domain) {
        switch(orient) {
        case "bottom":
        case "top":
            {
                return r.line({x1: scale(domain.min), x2: scale(domain.max), y1: 0, y2: 0});
            }
        case "right":
        case "left":
            {
                return r.line({x1: 0, x2: 0, y1: scale(domain.min), y2: scale(domain.max)});
            }
        }
    };
 
    // axis expects these props:
    // transform: a string in SVG transform format, relative to the parent : String
    // domain: The domain of values represented, with keys min, max : {}
    // scale: a function mapping the domain to SVG pixels : Num -> Num
    // default provided:
    // ticks: a function to pick tick positions, in the domain : domain -> scale -> [Num]
    // orient: one of 'top', 'bottom', 'right', 'left' : String
    // TODO handle right and top cases!
    // tickLength: Num
    // tickLabelSize: Num
    var axis = React.createClass({
        displayName: 'axis',
        render: function() {
            var props = this.props;

            var ticksList = props.ticks(props.domain, props.scale);

            return r.g(
                {transform: this.props.transform,
                stroke: 'black'},
                _.map(ticksList, tickMark(props.orient, props.scale, props.tickLength, props.tickLabelSize)),
                // axisLine
                axisLine(props.orient, props.scale, props.domain),
                _.map(ticksList, tickText(props.orient, props.scale, props.tickLength, props.tickLabelSize))
            );
        },
        getDefaultProps: function() {
            return {
                ticks: function(domain, scale) {
                    return scale.ticks(5);
                },
                orient: 'bottom',
                tickLength: 6,
                tickLabelSize: 12
            };
        }});

    // plot expects these props:
    // data: an array, element can be anything : [d]
    // width: SVG width in CSS pixels : Num
    // height: SVG height in CSS pixels : Num
    // with sensible defaults
    // margin: margin around the viewport (for title, axes) with attributes top, bottom, left, right : {}
    // xKey: a function to pick the x attribute : d -> Num (default _.property('x'))
    // yKey: a function to pick the y attribute : d -> Num (default _.property('y'))
    // xScale: a function to scale the x coord : Extent -> width  -> Num (default linear)
    // yScale:  a function to scale the y coord : Extent -> height  -> Num (default linear)
    return React.createClass({
        displayName: 'scatterPlot',
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
                }),
                axis({
                    transform: translate(props.margin.left, props.margin.top + viewHeight),
                    domain: extent(props.data, props.xKey),
                    scale: cx
                }),
                axis({
                    transform: translate(props.margin.left, props.margin.top),
                    domain: extent(props.data, props.yKey),
                    scale: cy,
                    orient: "left"
                })
            );
        },

        getDefaultProps: function() {
                return {
                   width: 450,
                    height: 300,
                    xKey: _.property('x'),
                    yKey: _.property('y'),
                    // xScale and yScale are functions
                    // xScale :: (value summary, viewport dim) -> x value -> output coördinate
                    xScale: function(data, w) {
                        return d3.scale.linear()
                            .domain([data.min, data.max])
                            .range([0, w]);
                    },
                    yScale: function(data, h) {
                        return d3.scale.linear().domain([data.min, data.max]).range([h,0]);
                    },
                    margin: {
                        left: 60,
                        right: 20,
                        top: 20,
                        bottom: 20
                    },
                    data: []
                };
            }});
});
