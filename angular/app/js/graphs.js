/* global d3 */

"use strict";

// Try to follow Bostock's proposal in http://bost.ocks.org/mike/chart/
// At least, make notes if deviating.

// in particular, I want to also take local vars as a dictionary.
// This is a huge amount of boilerplate.  It's probably worth the
// introspection magic to construct all these accessors
// programmatically, especially since the defaults will still be set
// simply.

// function lookup(map, key, def) {
//     if _.has(map, key) {
//         return map[key];
//     } else {
//         return def;
//     }};

function scatterplot(config) {
    // Default values, can be overridden by config or config() or individual setters
    var margin = {top: 20, right: 20, bottom: 30, left: 40};
    var width = 960 - margin.left - margin.right;
    var height = 500 - margin.top - margin.bottom;
    var datajson = [];
    var xAxisLabel = '';
    var yAxisLabel = '';
    var element = null;
    var xKey = 'x';
    var yKey = 'y';

    function render() {
        // Derived values, cannot be directly overridden
        var x = d3.scale.linear()
            .range([0, width]);
        var y = d3.scale.linear()
            .range([height, 0]);
        var xAxis = d3.svg.axis()
            .scale(x)
            .orient("bottom");
        var yAxis = d3.svg.axis()
            .scale(y)
            .orient("left");

        var svg = d3.select(element)
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        x.domain(d3.extent(datajson, function(d) { return d[xKey]; })).nice();
        y.domain(d3.extent(datajson, function(d) { return d[yKey]; })).nice();

        svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + height + ")")
            .call(xAxis)
            .append("text")
            .attr("class", "label")
            .attr("x", width)
            .attr("y", -6)
            .style("text-anchor", "end")
            .text(xAxisLabel);

        svg.append("g")
            .attr("class", "y axis")
            .call(yAxis)
            .append("text")
            .attr("class", "label")
            .attr("transform", "rotate(-90)")
            .attr("y", 6)
            .attr("dy", ".71em")
            .style("text-anchor", "end")
            .text(yAxisLabel);

        svg.selectAll(".dot")
            .data(datajson)
            .enter().append("circle")
            .attr("class", "dot")
            .attr("r", 2)
            .attr("cx", function(d) { return x(d[xKey]); })
            .attr("cy", function(d) { return y(d[yKey]); })
            .style("fill", "blue"); // TODO move this into CSS
    };

    render.config = function(config) {
        if (_.has(config, 'margin')) { margin = config['margin']; }
        if (_.has(config, 'width')) { width = config['width']; }
        if (_.has(config, 'height')) { height = config['height']; }
        if (_.has(config, 'datajson')) { datajson = config['datajson']; }
        if (_.has(config, 'xAxisLabel')) { xAxisLabel = config['xAxisLabel']; }
        if (_.has(config, 'yAxisLabel')) { yAxisLabel = config['yAxisLabel']; }
        if (_.has(config, 'element')) { element = config['element']; }
        if (_.has(config, 'xKey')) { xKey = config['xKey']; }
        if (_.has(config, 'yKey')) { yKey = config['yKey']; }
    };
    
    render.margin = function(v) {
        if (!arguments.length) { return margin; };
        margin = v;
        return render;
    };
    
    render.width = function(v) {
        if (!arguments.length) { return width; };
        width = v;
        return render;
    };
    
    render.height = function(v) {
        if (!arguments.length) { return height; };
        height = v;
        return render;
    };
    
    render.datajson = function(v) {
        if (!arguments.length) { return datajson; };
        datajson = v;
        return render;
    };

    render.xAxisLabel = function(v) {
        if (!arguments.length) { return xAxisLabel; };
        xAxisLabel = v;
        return render;
    };

    render.yAxisLabel = function(v) {
        if (!arguments.length) { return yAxisLabel; };
        yAxisLabel = v;
        return render;
    };

    render.element = function(v) {
        if (!arguments.length) { return element; };
        element = v;
        return render;
    };

    render.xKey = function(v) {
        if (!arguments.length) { return xKey; };
        xKey = v;
        return render;
    };

    render.yKey = function(v) {
        if (!arguments.length) { return yKey; };
        yKey = v;
        return render;
    };
    
    render.config(config);
    return render;
};
