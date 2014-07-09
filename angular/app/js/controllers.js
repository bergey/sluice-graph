/* global _, d3 */

'use strict';

/* Controllers */

angular.module('myApp.controllers', [])
    .controller(
        'BuildingList',
        [ '$scope', '$http',
          function($scope, $http) {
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
                  $scope.filterBldgs = function(sfMin, sfMax, sectors) {
                      return function(d) {
                          return sfMin < d.BLDG_FLOOR_AREA && d.BLDG_FLOOR_AREA < sfMax && sectors[d.SECTOR];
                      };
                  };
                  $scope.width = 800;
                  $scope.height = 400;
                  $scope.marginLeft = 25;
                  $scope.marginBottom = 25;
                  $scope.marginRight = 25;
                  $scope.marginTop = 25;
                  var x = d3.scale.linear().range(
                      [ 0, $scope.width-$scope.marginLeft-$scope.marginRight])
                      .domain([$scope.sfMin, $scope.sfMax]);
                  var y = d3.scale.linear().range(
                      [ $scope.height-$scope.marginTop-$scope.marginBottom, 0])
                      .domain(d3.extent(data, _.property("SITE_EUI")));
                  $scope.points = _.map(data, function(d) {
                      return { "x": x(d.BLDG_FLOOR_AREA), "y": y(d.SITE_EUI) };
                  });
              });
          }])
    .controller('euiSfCtrl', ['$scope', function($scope) {

    }])
    .controller('MyCtrl2', ['$scope', function($scope) {

    }]);
