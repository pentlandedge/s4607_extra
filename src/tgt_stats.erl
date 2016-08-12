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
-module(tgt_stats).

-export([
    extract/1, 
    dwell_dicts_to_geojson/1, 
    dwell_area_to_polygon/2,
    dwell_to_geojson/1,
    datetime_to_string/1,
    date_ms_to_datetime/2]).

-record(stat_acc, {ref_time, dwell_list}).

%% Function to extract the most useful fields relating to targets present in
%% a list of decoded s4607 packets.
%% Picks out the reference time from each passing mission segment and stores
%% it in each representation of a dwell segment.
%% Returns a list of dictionaries.
extract(PacketList) when is_list(PacketList) ->
    % Only interested in mission and dwell segments.
    Segs = s4607:get_segments_by_type([mission, dwell], PacketList),

    % Function to operate on each segment.
    F = fun(Seg, #stat_acc{ref_time = RT, dwell_list = DL} = AccStats) ->
            SH = segment:get_header(Seg),
            SegData = segment:get_data(Seg),
            T = seg_header:get_segment_type(SH),
            case T of
                mission ->
                    % Extract the time from the mission segment and update
                    % the current reference time.
                    NewRef = mission:get_time(SegData), 
                    AccStats#stat_acc{ref_time = NewRef};
                dwell ->
                    % Turn it into a dictionary.
                    DwellDict = dwell:to_dict(SegData),

                    % Add the mission time.
                    DwellParams = dict:store(mission_time, RT, DwellDict),

                    % Prepend to the list of dwells
                    NewDwellList = [DwellParams|DL],
                    AccStats#stat_acc{dwell_list = NewDwellList}
            end
        end,

    InitStats = #stat_acc{ref_time = 0, dwell_list = []}, 

    % Apply the fun over the list of segments.
    #stat_acc{dwell_list = Dwells} = lists:foldl(F, InitStats, Segs),
    
    % Reverse the list to return it to chronological order.
    lists:reverse(Dwells).

%% Function to convert a list of Dwell dictionaries (created by the extract/1
%% function above) to a list of GeoJSON records suitable to passing to a 
%% mapping client.
dwell_dicts_to_geojson(DwellList) when is_list(DwellList) ->
    PrepList = lists:map(fun dwell_dict_prep/1, DwellList),
    jsx:encode([
        {<<"data">>, PrepList}
    ]). 

%% Collect the relevant data into a structure suitable for encoding using 
%% the jsx library.
dwell_dict_prep(DwellDict) ->
    % Extract the mission time.
    MissTime = dict:fetch(mission_time, DwellDict),

    % Extract the dwell time (given in ms offset from the mission time).
    DwellTime = dict:fetch(dwell_time, DwellDict),

    % Convert to UTC (seconds precision).
    DwellUTC = tgt_stats:date_ms_to_datetime(MissTime, DwellTime),

    % Convert the UTC timestamp to a string.
    TimeStr = datetime_to_string(DwellUTC),
    
    % Extract the parameters related to the dwell area from the segment.
    DwellCentreLat = dict:fetch(dwell_center_lat, DwellDict),
    DwellCentreLon = dict:fetch(dwell_center_lon, DwellDict),
    DwellRangeHalfExtent = dict:fetch(dwell_range_half_extent, DwellDict),
    DwellAngleHalfExtent = dict:fetch(dwell_angle_half_extent, DwellDict),

    % Extract the sensor position paramters. We need these to be able to 
    % make sense of the dwell angle parameters when calculating the dwell
    % area extent on the ground.
    SensorLat = dict:fetch(sensor_lat, DwellDict),
    SensorLon = dict:fetch(sensor_lon, DwellDict),
    SensorAlt = dict:fetch(sensor_alt, DwellDict),

    %% Convert parameters to the appopriate units for calculation.
    DwellRangeHalfExtentMetres = km_to_m(DwellRangeHalfExtent),
    SensorAltMetres = cm_to_m(SensorAlt),

    DwellArea = {DwellCentreLat, DwellCentreLon, 
                 DwellRangeHalfExtentMetres, DwellAngleHalfExtent},

    SensorPos = {SensorLat, SensorLon, SensorAltMetres},

    {PtA, PtB, PtC, PtD} = dwell_area_to_polygon(DwellArea, SensorPos),
    
    % Fun to convert {Lat,Lon} to [Lon,Lat] form used by GeoJSON.
    F = fun(LatLon) ->
            LonLat = latlon_to_lonlat(LatLon),
            tuple_to_list(LonLat)
        end,

    % Get the target reports from the dwell.
    TgtReps = dict:fetch(targets, DwellDict),

    % Create a closure to wrap TimeStr. 
    TgtToGeoJSON = fun(T) -> target_dict_to_geojson(T, TimeStr) end,

    % Apply the function to the list of target dicts.
    TgtGeoList = lists:map(TgtToGeoJSON, TgtReps),

    % Construct the dwell area GeoGJON.
    DwellAreaGeo = dwell_area_to_geojson(TimeStr, F(PtA), F(PtB), F(PtC), F(PtD)),

    % Prepend the dwell area to the list of targets to create our features.
    FeatureList = [DwellAreaGeo|TgtGeoList],

    % Structure the whole lot for encoding and return to caller.
    [
        {<<"type">>,<<"FeatureCollection">>}, 
        {<<"features">>, FeatureList}
    ]. 

%% Function to convert a single dwell dictionary structure to the GeoJSON
%% form.
dwell_to_geojson(DwellDict) ->

    % Process the dwell dictionary.
    Collection = dwell_dict_prep(DwellDict),

    % Encode the whole lot and return to caller.
    jsx:encode(Collection). 

%% Convert distance in kilometres to metres.
km_to_m(Dist) -> Dist * 1000.

%% Convert cm to m.
cm_to_m(Dist) -> Dist / 100.

dwell_area_to_geojson(TimeStr, PtA, PtB, PtC, PtD) 
    when is_list(PtA), is_list(PtB), is_list(PtC), is_list(PtD) ->
    [{<<"type">>, <<"Feature">>},
        {<<"properties">>, [{<<"time">>, list_to_binary(TimeStr)}]},
        {<<"geometry">>, 
            [{<<"type">>, <<"Polygon">>}, 
                {<<"coordinates">>, [[PtA, PtB, PtC, PtD, PtA]]}
            ]
        }
    ].

%% @doc Function to convert the dwell area parameters into a bounding polygon
%% that can be displayed.
%% Ignore the altitude of the sensor for now and calculate the boundary 
%% points by applying the haversine formula to give the distance between two
%% Lat, Lon points along the arc of a great circle.
dwell_area_to_polygon(DwellArea, SensorPos) ->
    {CentreLat, CentreLon, RangeHE, AngleHE} = DwellArea,
    {SenLat, SenLon, _} = SensorPos,

    %% Need to check units of input parameters and convert to radians/metres
    %% as required.
    CentreDist = coord:haversine_distance({SenLat, SenLon}, {CentreLat, CentreLon}),
    CentreAngle = coord:initial_bearing({SenLat, SenLon}, {CentreLat, CentreLon}),
    NearDist = CentreDist - RangeHE,
    FarDist = CentreDist + RangeHE,

    PtA = coord:destination({SenLat, SenLon}, CentreAngle - AngleHE, NearDist),
    PtB = coord:destination({SenLat, SenLon}, CentreAngle - AngleHE, FarDist),
    PtC = coord:destination({SenLat, SenLon}, CentreAngle + AngleHE, FarDist),
    PtD = coord:destination({SenLat, SenLon}, CentreAngle + AngleHE, NearDist),

    {PtA, PtB, PtC, PtD}.

%% Extract the relevant fields from a target dict and produce a GeoJson form 
%% suitable for encoding.
target_dict_to_geojson(TgtDict, TimeStr) ->
    HrLat = dict:fetch(target_hr_lat, TgtDict), 
    HrLon = dict:fetch(target_hr_lon, TgtDict), 
    HrLonN = case HrLon > 180.0 of
                 true -> HrLon - 360.0;
                 false -> HrLon
             end,
    Height = dict:fetch(geodetic_height, TgtDict), 
    gen_tgt_geojson(TimeStr, HrLat, HrLonN, Height).

gen_tgt_geojson(Timestamp, Lat, Lon, Alt) ->
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"time">>, list_to_binary(Timestamp)}]},
     {<<"geometry">>, [{<<"type">>, <<"Point">>},
                       {<<"coordinates">>, [Lon, Lat, Alt]}]}
    ]. 

%% Function to convert the reference date from the mission segment to a 
%% string.
datetime_to_string({{Year,Month,Day},{Hours,Mins,Secs}}) ->
    Str = io_lib:format("~p-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
        [Year,Month,Day,Hours,Mins,Secs]),
    lists:flatten(Str).
    
%% Function to convert the reference date from the mission segment and the
%% offset in milliseconds contained in the dwell segment into a datetime 
%% structure.
date_ms_to_datetime({_Y, _M, _D} = Date, MS) ->
    RefSecs = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    TotalSecs = RefSecs + (MS div 1000),
    calendar:gregorian_seconds_to_datetime(TotalSecs).

%% Function to convert {Lat, Lon} tuples to {Lon, Lat} form used by GeoJson.
latlon_to_lonlat({Lat,Lon}) ->
    {Lon,Lat}.

