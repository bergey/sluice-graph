/* Each Component renders itself, based on specified properties and state.
 * Follow the D3 "fluent"style, rather than, say, the React style of setting properties, state.
 */

// A graph together with a form to restrict the displayed points.
// For now, lots of details of the data hardcoded in various places.
var filteredGraph = function() {
    // function for scope
    var state = {'data': none,
                 'xMin': none,
                 'xMax': none,
                 'yMin': none,
                 'yMax': none,
                 'sectors': none,
                 'selected': none};

    // utility function to create fluent Getter / Setter function
    var mkAccessor = function(obj, dict, key) {
        return function(val) {
            if (!arguments.length) {
                return dict[key];
            } else {
                dict[key] = val;
                return obj;
            }
        }
    };
    // add the specified accessor as a property of the object
    var addAccessor = function(obj, dict) {
        // curry for partial application in a map
        return function(key) {
            obj[key] = mkAccessor(obj, dict, key);
    }};
    
    var chart;
    nv.addGraph(function() {
        chart = nv.models.scatterChart()
            .useVoronoi(false)
            .transitionDuration(100)
        ;

        chart.xAxis.tickFormat(d3.format('.02f')).axisLabel('Floor Area [sf]');
        chart.yAxis.tickFormat(d3.format('.02f')).axisLabel('Energy Use Intensity [kBTU/yr/sf]');
        chart.tooltipContent(function(key, x, y, e, graph) {
            var details = e.series.values[e.pointIndex].details;
            show_details(details);
            return '';
        });
        
        d3.csv("Benchmarking_Data_Public.csv", function(data) {
            // chart.pointActive(filterBldgs().
            chart.xDomain(d3.extent(data, function(d) {return +d.BLDG_FLOOR_AREA;}));
            d3.select('#eui-vs-sf svg')
                .datum([{key: "Municipal Buildings", values: data.map(function(d) {return {x: +d.BLDG_FLOOR_AREA, y: +d.SITE_EUI, details: d};})}])
                .call(chart);
        });

        nv.utils.windowResize(chart.update);

        chart.dispatch.on('stateChange', function(e) { console.log('New State:', JSON.stringify(e)); });

        return chart;
    });

    var filterForm = function() {};
    var details = (function() {
        // properties
        var props = {'datum': none,
                     'element': none};

        var.r = {};  // return object
        
        var table_row = function(table, key, value) {
            table.append('tr').html('<td><strong>' + key + '</strong><td>' + value);
        };
        
        r.render = function() {
            props.element.html("");
            props.element.append("h3").text(props.datum.FACILITY_DESCRIPTION);
            props.element.append('p').html(props.datum.ASSET_ADDR + "<br />" + "Philadelphia, PA");
            var table = props.element.append('table');
            table_row(table, 'Build in', props.datum.YEAR_BUILT);
            table_row(table, 'Floor Area', props.datum.BLDG_FLOOR_AREA + ' square feet');
            table_row(table, 'Electricity', props.datum.ELECTRIC_USE + ' kWh per year');
            table_row(table, 'Gas', props.datum.GAS_USE);
        };

        _.each(_.keys(props), addAccessor(r, props)); // setup props accessors

        return r;
    })()

    // pick out the keys that the chart needs
    var chartKeys = function (d) {
        return {
            'x': d.BLDG_FLOOR_AREA,
            'y': d.SITE_EUI
            'id': d.ASSET_ID
        };
    };
    // predicate for filtering buildings
    var filterBldgs = function(d) {
        return (xMin <= d.BLDG_FLOOR_AREA
                && d.BLDG_FLOOR_AREA >= xMax
                && yMin <= d.SITE_EUI
                && d.SITE_EUI <= yMax
                && sectors[d.sector]);
    };
    
    var render = function() {
        // update all the internal props of the children
        chart.points(_.map(_.filter(data, filterBldgs), chartKeys));
        details.data()_.findWhere(data, {'id': selected});
        // call render on children
        chart.render();
        details.render();
    };

    render();
};

filteredGraph();

var filterBldgs = function() {
    var minSF = 0;
    var maxSF = 1e6;
    var sectors = {};

    function f(d) {
        return minSF <= d.x && d.x <= maxSF;
    }

    f.minSF = function (v) {
        if (!arguments.length) {
            return minSF;
        } else {
            minSf = v;
            return f;
        }};

    f.maxSF = function(v) {
        if (!arguments.length) {
            return maxSF;
        } else {
            maxSF = v;
            return f;
        }};

    f.sectors = function(v) {
        if (!arguments.length) {
            return sectors;
        } else {
            sectors = v;
            return f;
        }};

    return f;
}
    

d3.select('#minSF').on('change', function() {
    var d = chart.xDomain();
    d[0] = +this.value;
    chart.xDomain(d);
    chart.update();
});
    
d3.select('#maxSF')
    .on('change', function() {
        var d = chart.xDomain();
        d[1] = +this.value;
        chart.xDomain(d);
        chart.update();
    });

