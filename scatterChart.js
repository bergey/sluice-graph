function show_details(d) {
    var div = d3.select('#selected');
    div.html("");
    div.append("h3").text(d.FACILITY_DESCRIPTION);
    div.append('p').html(d.ASSET_ADDR + "<br />" + "Philadelphia, PA");
    var table = div.append('table');
    table_row(table, 'Build in', d.YEAR_BUILT);
    table_row(table, 'Floor Area', d.BLDG_FLOOR_AREA + ' square feet');
    table_row(table, 'Electricity', d.ELECTRIC_USE + ' kWh per year');
    table_row(table, 'Gas', d.GAS_USE);
}

function table_row(table, key, value) {
    table.append('tr').html('<td><strong>' + key + '</strong><td>' + value);
}

var filterBldgs = function() {
    f.minSF = 0;
    f.maxSF = 1e6;
    f.sectors = {};

    f = function(d) {
        return minSF <= d.x && d.x <= maxSF && sectors[d.details.SECTOR];
    }

    f.minSF = function (v) {
        if !arguments.length {
            return minSF;
        } else {
            minSf = v;
            return f;
        }};

    f.maxSF = function(v) {
        if !arguments.length {
            return maxSF;
        } else {
            maxSF = v;
            return f;
        }};

    f.sectors = function(v) {
        if !arguments.length {
            return sectors;
        } else {
            sectors = v;
            return f;
        }};

    return f;
}
    
//Format A
var chart;
nv.addGraph(function() {
  chart = nv.models.scatterChart()
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
      chart.pointActive(filterBldgs().minSF(d3.min(
      d3.select('#eui-vs-sf svg')
          .datum([{key: "Municipal Buildings", values: data.map(function(d) {return {x: +d.BLDG_FLOOR_AREA, y: +d.SITE_EUI, details: d};})}])
          .call(chart);
  });

  nv.utils.windowResize(chart.update);

  chart.dispatch.on('stateChange', function(e) { console.log('New State:', JSON.stringify(e)); });

  return chart;
});

d3.select('#minSF').on('change', function(val) {
    
