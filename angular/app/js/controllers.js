/* global _ */

'use strict';

/* Controllers */

angular.module('myApp.controllers', [])
    .controller('BuildingList', ['$scope', '$http', 
                                 function($scope, $http) {
                                     $http.get('Benchmarking_Data_Public.json').success(function(data) {
                                         // $scope.buildings=_.map(data, function(d) {
                                         //     d.eui = +d.SITE_EUI;
                                         //     d.sf = d.BLDG_FLOOR_AREA;
                                         //     return d;
                                         // });
                                         $scope.buildings = data;
                                         $scope.sectors = {};
                                         _.each(data, function(d) {
                                             if (d.SECTOR != "" && ! _.has($scope.sectors, d.SECTOR)) {
                                                 $scope.sectors[d.SECTOR] = true;
                                             }
                                        });
                                         $scope.sfMin = _.min(_.map(data, function(d) {return d.BLDG_FLOOR_AREA;}));
                                         $scope.sfGraphMin = $scope.sfMin;
                                         $scope.sfMax = _.max(_.map(data, function(d) {return d.BLDG_FLOOR_AREA;}));
                                         $scope.sfGraphMax = $scope.sfMax;
                                         $scope.filterBldgs = function(sfMin, sfMax, sectors) {
                                             return function(d) { 
                                                 return sfMin < d.BLDG_FLOOR_AREA && d.BLDG_FLOOR_AREA < sfMax && sectors[d.SECTOR]; 
                                             }
                                         };
                                     });
                                 }])
    .controller('MyCtrl1', ['$scope', function($scope) {

  }])
  .controller('MyCtrl2', ['$scope', function($scope) {

  }]);
