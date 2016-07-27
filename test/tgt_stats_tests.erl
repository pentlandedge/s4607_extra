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

-module(tgt_stats_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator function to run all the tests. 
tgt_stats_test_() ->
    [datetime_string_checks()].

datetime_string_checks() ->
    DT = {{2016,7,27},{8,12,59}},
    TimeStr = tgt_stats:datetime_to_string(DT),

    [?_assertEqual("2016-07-27 08:12:59", TimeStr)].
