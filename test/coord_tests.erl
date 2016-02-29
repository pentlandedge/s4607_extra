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
    [lla_to_ecef_checks()].

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

%% Utility function to compare whether floating point values are within a 
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
 
