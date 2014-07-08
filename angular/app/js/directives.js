'use strict';

/* Directives */


angular.module('myApp.directives', [])
  .directive('appVersion', ['version', function(version) {
    return function(scope, elm, attrs) {
      elm.text(version);
    };
  }])
    .directive('ngd3Scatter', function() {
        return {
            'restrict': 'A',
            'scope': {
                datajson: '=',
            },
            'link': function(scope, elem, attrs) {
                // TODO understand actual calling convention here
                // Should this work on every element?
                var plot = scatterplot(scope).xKey(attrs.xkey).yKey(attrs.ykey).xAxisLabel(attrs.xaxislabel).yAxisLabel(attrs.yaxislabel);
                plot.element(elem[0]);
                scope.$watch('datajson', function(newData, oldData) {
                    plot.datajson(newData)();
                })
            }
        };
    });
