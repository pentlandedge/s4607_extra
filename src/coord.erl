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

-export([lla_to_ecef/1, deg_to_rad/1, haversine_distance/2]).

%% WGS84 constants.
-define(WGS84_A, 6378137).
-define(WGS84_B, 6356752.31424518).

%% Mean radius of the earth used for haversine
-define(EARTH_MEAN_RAD, 6371000).

%% Function to convert from Lat, Lon, Alt to ECEF format
lla_to_ecef({Lat,Lon,Alt}) ->

    % Convert the Lat, Lon into radians.
    LatRad = deg_to_rad(Lat),
    LonRad = deg_to_rad(Lon),

    % Need to check that this height is from the correct reference.
    H = Alt,

    Asquared = ?WGS84_A * ?WGS84_A,
    Bsquared = ?WGS84_B * ?WGS84_B,
    Esquared = (Asquared - Bsquared) / Asquared, 

    SinLat = math:sin(LatRad),
    CosLat = math:cos(LatRad),
    SinLon = math:sin(LonRad),
    CosLon = math:cos(LonRad),

    % Calculate the radius of curvature
    N = ?WGS84_A / math:sqrt(1 - Esquared * SinLat * SinLat),

    % Calculate the coordinate points.
    X = (N + H) * CosLat * CosLon,
    Y = (N + H) * CosLat * SinLon,
    Z = ((Bsquared/Asquared) * N + H) * SinLat, 

    {X, Y, Z}.

deg_to_rad(Deg) ->
    Deg * math:pi() / 180.

%% Calculate the great circle distance between two points using the Haversine 
%% formula. Reference http://www.movable-type.co.uk/scripts/latlong.html.
haversine_distance({Lat1, Lon1}, {Lat2, Lon2}) ->
    LatRad1 = deg_to_rad(Lat1), 
    LonRad1 = deg_to_rad(Lon1), 
    LatRad2 = deg_to_rad(Lat2), 
    LonRad2 = deg_to_rad(Lon2), 
    
    DeltaLat = LatRad2 - LatRad1, 
    DeltaLon = LonRad2 - LonRad1, 

    SinHalfDLat = math:sin(DeltaLat/2),
    SinSqHalfDLat = SinHalfDLat * SinHalfDLat,  

    SinHalfDLon = math:sin(DeltaLon/2),
    SinSqHalfDLon = SinHalfDLon * SinHalfDLon, 

    A = SinSqHalfDLat + math:cos(LatRad1) * math:cos(LatRad2) * SinSqHalfDLon, 
    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1-A)),
    %Distance = ?WGS84_A * C,
    Distance = ?EARTH_MEAN_RAD * C,
    Distance.

