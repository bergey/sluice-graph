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

//Format A
var chart;
nv.addGraph(function() {
  chart = nv.models.scatterChart()
                .showDistX(true)
                .showDistY(true)
                .useVoronoi(true)
                .transitionDuration(300)
                ;

  chart.xAxis.tickFormat(d3.format('.02f')).axisLabel('Floor Area [sf]');
  chart.yAxis.tickFormat(d3.format('.02f')).axisLabel('Energy Use Intensity [kBTU/yr/sf]');
  chart.tooltipContent(function(key, x, y, e, graph) {
      var details = e.series.values[e.pointIndex].details;
      show_details(details);
      console.log(details);
      return '<h2>' + details.FACILITY_DESCRIPTION + '</h2>';
  });
  
  d3.csv("Benchmarking_Data_Public.csv", function(data) {
      d3.select('#eui-vs-sf svg')
          .datum([{key: "Municipal Buildings", values: data.map(function(d) {return {x: +d.BLDG_FLOOR_AREA, y: +d.SITE_EUI, details: d};})}])
          .call(chart);
  });

  nv.utils.windowResize(chart.update);

  chart.dispatch.on('stateChange', function(e) { ('New State:', JSON.stringify(e)); });

  return chart;
});
