var app = angular.module('MainApp', ['ngRpc']);

app.config(['$rpcProvider', function($rpcProvider){
    $rpcProvider.setUrl('ws://localhost:8008/bullet');
    $rpcProvider.connect();
}]);

app.controller('MainCtrl', ['$scope', '$rpc',  function($scope, $rpc) {
    $scope.first = 0;
    $scope.second = 0;
    $scope.result = 0;

    $scope.add = function() {
        $rpc.send('add', [ parseInt($scope.first), parseInt($scope.second) ]).then(
            function(response) {
                console.log('Success', response); 
                $scope.result = response.result;
            },
            function(response) {
                console.log('Error', response);
            }
        );
    }
    $scope.add_delay = function() {
        $rpc.send('add_delay', [ parseInt($scope.first), parseInt($scope.second) ]).then(
            function(response) {
                console.log('Success', response); 
                $scope.result = response.result;
            },
            function(response) {
                console.log('Error', response);
            }
        );
    }
}]);

