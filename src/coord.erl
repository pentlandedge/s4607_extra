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
-module(coord).

-export([lla_to_ecef/1]).

%% WGS84 constants.
-define(WGS84_A, 6378137).
-define(WGS84_B, 6356752.31424518).

%% Function to convert from Lat, Lon, Alt to ECEF format
lla_to_ecef({Lat,Lon,Alt}) ->

    % Need to check that this height is from the correct reference.
    H = Alt,

    Asquared = ?WGS84_A * ?WGS84_A,
    Bsquared = ?WGS84_B * ?WGS84_B,
    Esquared = (Asquared - Bsquared) / Asquared, 

    SinLat = math:sin(Lat),
    CosLat = math:cos(Lat),
    SinLon = math:sin(Lon),
    CosLon = math:cos(Lon),

    % Calculate the radius of curvature
    N = ?WGS84_A / math:sqrt(1 - Esquared * SinLat * SinLat),

    % Calculate the coordinate points.
    X = (N + H) * CosLat * CosLon,
    Y = (N + H) * CosLat * SinLon,
    Z = ((Bsquared/Asquared) * N + H) * SinLat, 

    {X, Y, Z}.


