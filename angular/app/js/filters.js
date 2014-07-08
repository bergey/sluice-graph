'use strict';

/* Filters */

angular.module('myApp.filters', []).
  filter('interpolate', ['version', function(version) {
    return function(text) {
      return String(text).replace(/\%VERSION\%/mg, version);
    };
  }])
.filter('filterBldgs', function() {
    return function(bldgs, sfMin, sfMax, sectors) {
        console.log("filtering buildings on " + sfMin + " < sf < " + sfMax + ", " + sectors.toString());
/*        return _.filter(bldgs, function(d) {
            return sfMin < d.BLDG_FLOOR_AREA && d.BLDG_FLOOR_AREA < sfMax && sectors[d.SECTOR];
        })
*/
        var ret = [];
        bldgs.forEach(function(d) {
            if (sfMin < d.BLDG_FLOOR_AREA && d.BLDG_FLOOR_AREA < sfMax && sectors[d.SECTOR]) {
                ret.push(d);
            }
        });
        return ret;
    };
});
