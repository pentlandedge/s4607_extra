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
    extract_scans/1,
    dwell_dicts_to_geojson/1,
    dwell_area_to_polygon/2,
    dwell_to_geojson/1,
    datetime_to_string/1,
    date_ms_to_datetime/2,
    date_ms_to_utc/2,
    get_bounding_area/1,
    job_def_to_polygon/1,
    group_dwells_by_revisit/1]).

-record(stat_acc, {ref_time, last_job_def, dwell_list}).

%% Function to extract the most useful fields relating to targets present in
%% a list of decoded s4607 packets.
%% Picks out the reference time from each passing mission segment and stores
%% it in each representation of a dwell segment.
%% Returns a list of dictionaries.
extract(PacketList) when is_list(PacketList) ->
    % Only interested in mission and dwell segments.
    Segs = s4607:get_segments_by_type([mission, dwell], PacketList),

    % Process the list of segments, accumulating statistics.
    InitStats = #stat_acc{ref_time = 0, dwell_list = []},
    #stat_acc{dwell_list = Dwells} =
        lists:foldl(fun process_segment/2, InitStats, Segs),

    % Reverse the list to return it to chronological order.
    lists:reverse(Dwells).

%% Version of extract which operates on scans (a collection of dwells grouped
%% by revisit).
extract_scans(PacketList) when is_list(PacketList) ->

    % Interested in job definition, mission and dwell segments.
    SegTypes = [job_definition, mission, dwell],
    _Segs = s4607:get_segments_by_type(SegTypes, PacketList),

    ok.

%% Function to operate on each segment, extracting the required information.
%% Accumulates statistics, designed to work with a fold.
process_segment(Seg, #stat_acc{} = AccStats) ->
    SH = segment:get_header(Seg),
    SegData = segment:get_data(Seg),
    T = seg_header:get_segment_type(SH),
    process_seg_data(T, SegData, AccStats).

%% Process the data field of the relevant segment types.
process_seg_data(job_definition, SegData, #stat_acc{} = AccStats) ->
    AccStats#stat_acc{last_job_def = SegData};
process_seg_data(mission, SegData, #stat_acc{} = AccStats) ->
    % Extract the time from the mission segment and update the current
    % reference time.
    NewRef = mission:get_time(SegData),
    AccStats#stat_acc{ref_time = NewRef};
process_seg_data(dwell, SegData,
    #stat_acc{ref_time = RT, dwell_list = DL} = AccStats) ->

    % Turn it into a dictionary.
    DwellDict = dwell:to_dict(SegData),

    % Add the mission time and prepend to the list of dwells.
    DwellParams = dict:store(mission_time, RT, DwellDict),
    NewDwellList = [DwellParams|DL],
    AccStats#stat_acc{dwell_list = NewDwellList};
process_seg_data(_, _, #stat_acc{} = AccStats) ->
    AccStats.

%% Function to convert a list of Dwell dictionaries (created by the extract/1
%% function above) to a list of GeoJSON records suitable to passing to a
%% mapping client.
dwell_dicts_to_geojson(DwellList) when is_list(DwellList) ->
    PrepList = lists:map(fun dwell_dict_prep/1, DwellList),
    jsx:encode([{<<"data">>, PrepList}]).

%% Collect the relevant data into a structure suitable for encoding using
%% the jsx library.
dwell_dict_prep(DwellDict) ->
    % Calculate the dwell time (UTC)and convert the UTC timestamp to a string.
    DwellUTC = calculate_dwell_utc_time(DwellDict),
    TimeStr = datetime_to_string(DwellUTC),
    TimeUtc = calculate_dwell_utc_time_ms(DwellDict),
    % Extract the sensor position and dwell area parameters and use these to
    % calculate the vertices of the dwell polygon.
    SensorPos = get_sensor_position(DwellDict),
    DwellArea = get_dwell_area(DwellDict),
    {PtA, PtB, PtC, PtD} = dwell_area_to_polygon(DwellArea, SensorPos),

    % Get the target reports from the dwell and convert the list to GeoJSON
    % encoding form. Uses a closure to wrap TimeStr for the map operation.
    TgtReps = dict:fetch(targets, DwellDict),
    TgtToGeoJSON = fun(T) -> target_dict_to_geojson(T, TimeStr, TimeUtc) end,
    TgtGeoList = lists:map(TgtToGeoJSON, TgtReps),

    % Form the dwell area GeoJSON and construct our list of features.
    DwellAreaGeo = dwell_area_to_geojson(TimeStr, TimeUtc, PtA, PtB, PtC, PtD),
    FeatureList = [DwellAreaGeo|TgtGeoList],

    % Structure the whole lot for encoding and return to caller.
    [{<<"type">>,<<"FeatureCollection">>},
     {<<"features">>, FeatureList}].

%% Calculate the dwell UTC time from the mission base and dwell offset.
calculate_dwell_utc_time(DwellDict) ->
    % Extract the mission and dwell times.
    MissTime = dict:fetch(mission_time, DwellDict),
    DwellTime = dict:fetch(dwell_time, DwellDict),

    % Convert to UTC (seconds precision).
    tgt_stats:date_ms_to_datetime(MissTime, DwellTime).

%% Calculate the dwell UTC time from the mission base and dwell offset.
calculate_dwell_utc_time_ms(DwellDict) ->
    % Extract the mission and dwell times.
    MissTime = dict:fetch(mission_time, DwellDict),
    DwellTime = dict:fetch(dwell_time, DwellDict),

    % Convert to UTC (seconds precision).
    tgt_stats:date_ms_to_utc(MissTime, DwellTime).

%% Convert a single dwell dictionary structure to the GeoJSON form.
dwell_to_geojson(DwellDict) ->
    % Process the dwell dictionary, and encode it in JSON form.
    Collection = dwell_dict_prep(DwellDict),
    jsx:encode(Collection).

%% Extract the dwell area parameters and convert to standard units.
get_dwell_area(DwellDict) ->
    % Extract the parameters related to the dwell area from the segment.
    CentreLat = dict:fetch(dwell_center_lat, DwellDict),
    CentreLon = dict:fetch(dwell_center_lon, DwellDict),
    RangeHalfExtent = dict:fetch(dwell_range_half_extent, DwellDict),
    AngleHalfExtent = dict:fetch(dwell_angle_half_extent, DwellDict),

    %% Convert parameters to the appopriate units for calculation.
    RangeHalfExtentMetres = km_to_m(RangeHalfExtent),

    {CentreLat, CentreLon, RangeHalfExtentMetres, AngleHalfExtent}.

%% Extract the sensor position from the dwell dict in standard units
%% (altitude converted to metres).
get_sensor_position(DwellDict) ->
    SensorLat = dict:fetch(sensor_lat, DwellDict),
    SensorLon = dict:fetch(sensor_lon, DwellDict),
    SensorAlt = dict:fetch(sensor_alt, DwellDict),

    %% Convert parameters to the appopriate units for calculation.
    SensorAltMetres = cm_to_m(SensorAlt),

    {SensorLat, SensorLon, SensorAltMetres}.

%% Convert distance in kilometres to metres.
km_to_m(Dist) -> Dist * 1000.

%% Convert a distance in centimetres to metres.
cm_to_m(Dist) -> Dist / 100.

%% Convert the dwell area  parameters to a form suitable for GeoJSON encoding.
dwell_area_to_geojson(TimeStr, TimeUtc, LatLonA, LatLonB, LatLonC, LatLonD) ->
    PtA = latlon_tuple_to_lonlat_list(LatLonA),
    PtB = latlon_tuple_to_lonlat_list(LatLonB),
    PtC = latlon_tuple_to_lonlat_list(LatLonC),
    PtD = latlon_tuple_to_lonlat_list(LatLonD),
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"time">>, list_to_binary(TimeStr)},
                        {<<"start">>, TimeUtc},
                        {<<"end">>, TimeUtc}]},
     {<<"geometry">>, [{<<"type">>, <<"Polygon">>},
                       {<<"coordinates">>, [[PtA, PtB, PtC, PtD, PtA]]}]}].

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

%% Extract the relevant fields from a target dict and produce a form suitable
%% suitable for GeoJSON encoding.
target_dict_to_geojson(TgtDict, TimeStr, TimeUtc) ->
    HrLat = dict:fetch(target_hr_lat, TgtDict),
    HrLon = dict:fetch(target_hr_lon, TgtDict),
    HrLonN = case HrLon > 180.0 of
                 true -> HrLon - 360.0;
                 false -> HrLon
             end,
    Height = dict:fetch(geodetic_height, TgtDict),
    gen_tgt_geojson(TimeStr, TimeUtc, HrLat, HrLonN, Height).

%% Convert the target fields to a form suitable for GeoJSON encoding.
gen_tgt_geojson(Timestamp, TimeUtc, Lat, Lon, Alt) ->
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"time">>, list_to_binary(Timestamp)},
                        {<<"start">>, TimeUtc},
                        {<<"end">>, TimeUtc}]},
     {<<"geometry">>, [{<<"type">>, <<"Point">>},
                       {<<"coordinates">>, [Lon, Lat, Alt]}]}].

%% Convert the reference date from the mission segment to a string.
datetime_to_string({{Year,Month,Day},{Hours,Mins,Secs}}) ->
    Str = io_lib:format("~p-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
        [Year,Month,Day,Hours,Mins,Secs]),
    lists:flatten(Str).

%% Convert the reference date from the mission segment and the offset in
%% milliseconds contained in the dwell segment into a datetime structure.
date_ms_to_datetime({_Y, _M, _D} = Date, MS) ->
    RefSecs = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    TotalSecs = RefSecs + (MS div 1000),
    calendar:gregorian_seconds_to_datetime(TotalSecs).

%% Convert the reference date from the mission segment and the offset in
%% milliseconds contained in the dwell segment into a epoch.
date_ms_to_utc({_Y, _M, _D} = Date, MS) ->
    RefSecs = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    %% Subtract the number of seconds for Unix Epoch time
    (RefSecs - 62167219200) *1000 + MS.

%% Convert {Lat, Lon} tuples to {Lon, Lat} form used by GeoJSON.
latlon_to_lonlat({Lat,Lon}) ->
    {Lon,Lat}.

%% Convert a tuple of {Lat, Lon} to a list [Lon, Lat] form.
latlon_tuple_to_lonlat_list({_Lat, _Lon} = LatLon) ->
    LonLat = latlon_to_lonlat(LatLon),
    tuple_to_list(LonLat).

%% Extract the bounding area from a job definiton segment.
get_bounding_area(JD) ->
    A = {job_def:get_bounding_a_lat(JD), job_def:get_bounding_a_lon(JD)},
    B = {job_def:get_bounding_b_lat(JD), job_def:get_bounding_b_lon(JD)},
    C = {job_def:get_bounding_c_lat(JD), job_def:get_bounding_c_lon(JD)},
    D = {job_def:get_bounding_d_lat(JD), job_def:get_bounding_d_lon(JD)},
    {A, B, C, D}.

%% Convert the job defintion bounding area into the polygon form. Simple
%% mapping, one of the functions should probably be removed. 
job_def_to_polygon(JobDef) ->
    get_bounding_area(JobDef).

%% Function to accumulate dwell segmemts from a packet list into groups based
%% around the revisit index. This function is doing several list traversals
%% and constructions. It could be optimised if required.
group_dwells_by_revisit(PacketList) when is_list(PacketList) ->
    Segs = s4607:get_segments_by_type([dwell], PacketList),
    {Grouped, Rem} = lists:foldl(fun acc_dwells/2, {[], []}, Segs),
    {ok, lists:reverse(Grouped), lists:reverse(Rem)}.

%% Function to be used with a fold. It accumulates dwell segments into 
%% groups based on the last dwell of revisit.
acc_dwells(DwellSeg, {GroupedList, CurrentRevisit}) when is_list(GroupedList), 
    is_list(CurrentRevisit) ->
    
    DwellData = segment:get_data(DwellSeg),
    NewCurrent = [DwellData|CurrentRevisit],
    case dwell:get_last_dwell_of_revisit(DwellData) of
        no_additional_dwells ->
            {[lists:reverse(NewCurrent)|GroupedList], []};
        additional_dwells ->
            {GroupedList, NewCurrent}
    end.

