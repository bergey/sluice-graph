/* global _, d3 */

'use strict';

/* Controllers */

angular.module('myApp.controllers', [])
    .filter('filterBldgs', function () {
        return function(bldgs, sfMin, sfMax, sectors) {
            return _.filter(bldgs,  function(d) {
                return sfMin < d.BLDG_FLOOR_AREA && d.BLDG_FLOOR_AREA < sfMax && sectors[d.SECTOR];
            });
        };})
    .filter('toPoint', function () {
        return function(bldgs,width,height,left,right,top,bottom) {
            var x = d3.scale.linear()
                .range([ 0, width - left - right])
                .domain(d3.extent(bldgs, _.property("BLDG_FLOOR_AREA")));
            var y = d3.scale.linear()
                .range([ height - top - bottom, 0])
                .domain(d3.extent(bldgs, _.property("SITE_EUI")));
            var ret =  _.map(bldgs, function(d) {
                return {
                    "x": x(d.BLDG_FLOOR_AREA),
                    "y": y(d.SITE_EUI) ,
                    "id": d.ASSET_ID
                };});
            return ret;
            };})
    .controller(
        'BuildingList',
        [ '$scope', '$http', 'filterBldgsFilter',
          function($scope, $http, filterBldgsFilter) {
              $http.get('Benchmarking_Data_Public.json').success(function(data) {
                  $scope.buildings = data;
                  // populate sectors checkboxes with fields appearing in data
                  $scope.sectors = {};
                  _.each(data, function(d) {
                      if (d.SECTOR !== "" && ! _.has($scope.sectors, d.SECTOR)) {
                          $scope.sectors[d.SECTOR] = true;
                      }
                  });
                  // populate square footage fields with range of data
                  $scope.sfMin = _.min(_.map(data, function(d) {return d.BLDG_FLOOR_AREA;}));
                  $scope.sfGraphMin = $scope.sfMin;
                  $scope.sfMax = _.max(_.map(data, function(d) {return d.BLDG_FLOOR_AREA;}));
                  $scope.sfGraphMax = $scope.sfMax;
                  $scope.width = 800;
                  $scope.height = 400;
                  $scope.marginLeft = 25;
                  $scope.marginBottom = 25;
                  $scope.marginRight = 25;
                  $scope.marginTop = 25;
              });
          }])
    .controller('euiSfCtrl', ['$scope', function($scope) {

    }])
    .controller('MyCtrl2', ['$scope', function($scope) {

    }]);
