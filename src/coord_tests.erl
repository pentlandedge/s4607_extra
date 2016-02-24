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
    {X1,Y1,Z1} = coord:lla_to_ecef({0,0,0}),

    [?_assert(almost_equal(6378137.0, X1, 0.001)),
     ?_assert(almost_equal(0.0, Y1, 0.001)),
     ?_assert(almost_equal(0.0, Z1, 0.001))].

%% Utility function to compare whether floating point values are within a 
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.
 
