%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2016 Pentland Edge Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. 
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software 
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT 
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the 
%% License for the specific language governing permissions and limitations 
%% under the License.
%%

-module(coord_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator function to run all the tests. 
coord_test_() ->
    [lla_to_ecef_checks(), ecef_distance_checks(), haversine_checks(), 
     initial_bearing_checks(), destination_checks(),
     ecef_to_enu_checks()].

lla_to_ecef_checks() ->
    % Start with a point on the equator.
    {X1,Y1,Z1} = coord:lla_to_ecef({0,0,0}),

    % Try a point in the northern hemisphere, west of the dateline.
    {X2,Y2,Z2} = coord:lla_to_ecef({45,350,1000}),

    % Try a point in the southern hemisphere, east of the dateline, height 
    % below the ellipsoid.
    {X3,Y3,Z3} = coord:lla_to_ecef({-45,150,-300}),

    % Try a point as far south as we can go.
    {X4,Y4,Z4} = coord:lla_to_ecef({-180,0.0,0.0}),

    [?_assert(almost_equal(6378137.0, X1, 0.1)),
     ?_assert(almost_equal(0.0, Y1, 0.001)),
     ?_assert(almost_equal(0.0, Z1, 0.001)),
     ?_assert(almost_equal(4449655, X2, 1)),
     ?_assert(almost_equal(-784594, Y2, 1)),
     ?_assert(almost_equal(4488056, Z2, 1)),
     ?_assert(almost_equal(-3912165, X3, 1)),
     ?_assert(almost_equal(2258689, Y3, 1)),
     ?_assert(almost_equal(-4487136, Z3, 1)),
     ?_assert(almost_equal(-6378137, X4, 1)),
     ?_assert(almost_equal(0.0, Y4, 0.001)),
     ?_assert(almost_equal(0.0, Z4, 0.001))].

ecef_distance_checks() ->
    Dist = coord:ecef_distance({45,-20,88}, {-13,44,200}),
    [?_assert(almost_equal(141.435498, Dist, 0.0001))].

haversine_checks() ->
    Pt1 = {55.9987, -2.71},
    Pt2 = {56.001, -2.734},
    Dist = coord:haversine_distance(Pt1, Pt2),

    [?_assert(almost_equal(1514, Dist, 1))].

initial_bearing_checks() ->
    Pt1 = {55.9987, -2.71},
    Pt2 = {56.001, -2.734},
    Bearing = coord:initial_bearing(Pt1, Pt2),

    [?_assert(almost_equal(279.735, Bearing, 0.001))].

destination_checks() ->
    Pt1 = {55.9987, -2.71},
    Bearing = 279.735,
    Distance = 1514,

    Destination = coord:destination(Pt1, Bearing, Distance),
    {Lat, Lon} = Destination,

    [?_assert(almost_equal(56.001, Lat, 0.001)),
     ?_assert(almost_equal(-2.734, Lon, 0.001))].

ecef_to_enu_checks() ->
    % Take a couple of points that are close together on the surface of the
    % earth. Compute the distance between them using two separate methods
    % and check that the answers are not too far apart.
    Pt1 = {55.9987, -2.71, 0},
    Pt2 = {55.9986, -2.71, 0},
    Pt1ECEF = coord:lla_to_ecef(Pt1),
    Pt2ECEF = coord:lla_to_ecef(Pt2),
    DistECEF = coord:ecef_distance(Pt1ECEF, Pt2ECEF),
    Pt1ENU = coord:ecef_to_enu(Pt1, Pt1ECEF),  
    Pt2ENU = coord:ecef_to_enu(Pt1, Pt2ECEF),  
    DistENU = coord:enu_distance(Pt1ENU, Pt2ENU),
    % Add a third point.
    Pt3 = {55.9987, -2.7101, 0},
    Pt3ECEF = coord:lla_to_ecef(Pt3),
    DistECEF3 = coord:ecef_distance(Pt1ECEF, Pt3ECEF),
    Pt3ENU = coord:ecef_to_enu(Pt1, Pt3ECEF),
    DistENU3 = coord:enu_distance(Pt1ENU, Pt3ENU),
    % Calculate distances between 2 and 3.
    DistECEF23 = coord:ecef_distance(Pt2ECEF, Pt3ECEF),
    DistENU23 = coord:enu_distance(Pt2ENU, Pt3ENU),
    {E1,N1,_U1} = Pt1ENU, 
    {E2,N2,_U2} = Pt2ENU, 
    {E3,_N3,_U3} = Pt3ENU, 
    [?_assert(almost_equal(DistECEF, DistENU, 0.001)),
     ?_assert(almost_equal(DistECEF3, DistENU3, 0.001)),
     ?_assert(almost_equal(DistECEF23, DistENU23, 0.001)),
     ?_assert(N1 > N2),
     ?_assert(E1 > E3),
     ?_assert(E2 > E3)
    ].

%% Utility function to compare whether floating point values are within a 
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
 
