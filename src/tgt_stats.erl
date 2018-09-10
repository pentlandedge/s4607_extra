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
    new_scan/0,
    extract/1,
    accumulate_updates/1,
    accumulate_scans/1,
    accumulate_scans/3,
    scans_to_geojson/1,
    scan_prep/1,
    calculate_scan_start_utc_time/1,
    get_targets_from_scan/1,
    dwell_area_to_polygon/2,
    datetime_to_string/1,
    date_ms_to_datetime/2,
    date_ms_to_utc/2,
    get_bounding_area/1,
    job_def_to_polygon/1,
    group_dwells_by_revisit/1,
    grouped_dwells_to_polygon/1,
    fuse_polygons/1]).

%% Accessor functions for manipulating location updates.
-export([get_last_mission/1, get_loc_data/1]).

%% Accessor functions for scans.
-export([get_grouped_dwells/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Record definitions. 

-record(stat_acc, {ref_time, last_job_def, dwell_list}).

%% Accumulating data as "scans" which are grouped by revisit.
-record(scan, 
    {last_mission = none, 
     last_job_def = none, 
     grouped_dwells = []}).

%% Record definition to hold information relating to a platform location
%% update.
-record(loc_update, {last_mission = none, loc_data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type specifications.

-opaque scan() :: #scan{}.
-opaque loc_update() :: #loc_update{}.

%% Create a generic type which can be extended to contain various types of 
%% information from the platform.
-type platform_update() :: {scan, scan()} | {loc_update, loc_update()}.

-export_type([scan/0, loc_update/0, platform_update/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function declarations.

%% Return an empty scan.
new_scan() ->
    #scan{}.

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

%% @doc New interface intended to replace accumulate_scans/1. Works over a 
%% list of packets and accumulates generic "platform updates". This is 
%% to allow collection of other information of interest such as platform
%% location updates during periods when the radar is not being used.
-spec accumulate_updates(PL::list()) -> [platform_update()].
accumulate_updates(PacketList) when is_list(PacketList) ->
    Segs = s4607:get_segments(PacketList),
    {_, Updates} = lists:foldl(fun acc_updates/2, {none, []}, Segs),
    lists:reverse(Updates).

acc_updates(Seg, UpdateAcc) ->
    SH = segment:get_header(Seg),
    SegData = segment:get_data(Seg),
    T = seg_header:get_segment_type(SH),
    proc_seg2(T, SegData, UpdateAcc).

proc_seg2(mission, SegData, {_, Updates}) ->
    {SegData, Updates};
proc_seg2(platform_loc, SegData, {LastMiss, Updates}) -> 
    Update = {loc_update, #loc_update{last_mission = LastMiss, loc_data = SegData}},
    NewUpdates = [Update|Updates], 
    {LastMiss, NewUpdates};
proc_seg2(dwell, SegData, {LastMiss, Updates}) -> 
    Update = {scan, #scan{last_mission = LastMiss, 
                          last_job_def = none, 
                          grouped_dwells = [SegData]}},
    NewUpdates = [Update|Updates], 
    {LastMiss, NewUpdates};
proc_seg2(_, _, UpdateAcc) -> 
    UpdateAcc.

accumulate_scans(PacketList) when is_list(PacketList) ->
    accumulate_scans(PacketList, #scan{}, []).

accumulate_scans(PacketList, PartScanIn, DwellsIn) when is_list(PacketList) ->
    Segs = s4607:get_segments(PacketList),
    ScanAcc  = lists:foldl(fun acc_scans/2, {[], PartScanIn, DwellsIn}, Segs),
    {Scans, PartScan, Dwells} = ScanAcc,
    NewScanList = lists:reverse(Scans),
    {NewScanList, PartScan, Dwells}.

acc_scans(Seg, ScanAcc) ->
    SH = segment:get_header(Seg),
    SegData = segment:get_data(Seg),
    T = seg_header:get_segment_type(SH),
    proc_seg(T, SegData, ScanAcc).

proc_seg(mission, SegData, {Scans, CurrScan, Dwells}) ->
    {Scans, CurrScan#scan{last_mission = SegData}, Dwells};
proc_seg(job_definition, SegData, {Scans, CurrScan, Dwells}) ->
    {Scans, CurrScan#scan{last_job_def = SegData}, Dwells};
proc_seg(dwell, SegData, {Scans, CurrScan , Dwells}) -> 
    NewCurrent = [SegData|Dwells],
    case dwell:get_last_dwell_of_revisit(SegData) of
        no_additional_dwells ->
            CompleteScan = CurrScan#scan{grouped_dwells = lists:reverse(NewCurrent)},
            % We preserve the mission and job def segs for the next scan.
            {[CompleteScan|Scans], CurrScan, []};
        additional_dwells ->
            {Scans, CurrScan, NewCurrent} 
    end;
proc_seg(platform_loc, _, ScanAcc) -> 
    ScanAcc;
proc_seg(_, _, ScanAcc) -> 
    ScanAcc.


%% Function to convert a scan's worth of data to geojson.
scans_to_geojson(ScanList) when is_list(ScanList)  ->
    PrepList = lists:map(fun scan_prep/1, ScanList),
    jsx:encode([{<<"data">>, PrepList}]).

%% Collect the relevant data into a structure suitable for encoding using
%% the jsx library.
scan_prep(#scan{grouped_dwells = GD} = Scan) -> 

    % Calculate the dwell time (UTC)and convert the UTC timestamp to a string.
    DwellUTC = calculate_scan_start_utc_time(Scan),
    TimeStr = datetime_to_string(DwellUTC),
    TimeUtc = calculate_scan_utc_time_ms(Scan),
    % Extract the sensor position and dwell area parameters and use these to
    % calculate the vertices of the dwell polygon.
    SensorPos = get_sensor_position_from_scan(Scan),
    SensorGeo = sensor_to_geojson(SensorPos, TimeStr, TimeUtc),

    PointList = grouped_dwells_to_polygon(GD),

    % Get the target reports from the dwell and convert the list to GeoJSON
    % encoding form. Uses a closure to wrap TimeStr and TimeUTC for the map 
    % operation.
    TgtReps = get_targets_from_scan(Scan),
    TgtToGeoJSON = fun(T) -> target_to_geojson(T, TimeStr, TimeUtc) end,
    TgtGeoList = lists:map(TgtToGeoJSON, TgtReps),

    % Form the scan area GeoJSON and construct our list of features.
    ScanAreaGeo = scan_polygon_to_geojson(TimeStr, TimeUtc, PointList),
    FeatureList = [ScanAreaGeo|[SensorGeo|TgtGeoList]],

    % Structure the whole lot for encoding and return to caller.
    [{<<"type">>,<<"FeatureCollection">>},
     {<<"features">>, FeatureList}].

%% Calculate the scan start UTC time from the mission base and dwell offset.
calculate_scan_start_utc_time( 
    #scan{last_mission = M, grouped_dwells = GD} = _Scan) ->

    % Extract the mission and dwell times.
    % We may not have received a mission segment, default to 1970 epoch to 
    % make it clear that the data is invalid. 
    case M of 
        none -> 
            MissTime = {1970, 1, 1};
        _    ->
            MissTime = mission:get_time(M)
    end, 

    [FirstDwell|_Rest] = GD,
    DwellTime = dwell:get_dwell_time(FirstDwell),

    % Convert to UTC (seconds precision).
    tgt_stats:date_ms_to_datetime(MissTime, DwellTime).


%% Calculate the dwell UTC time from the mission base and dwell offset.
calculate_scan_utc_time_ms(
    #scan{last_mission = M, grouped_dwells = GD} = _Scan) ->

    % Extract the mission and dwell times.
    case M of 
        none -> 
            MissTime = {1970, 1, 1};
        _    ->
            MissTime = mission:get_time(M)
    end, 
    [FirstDwell|_Rest] = GD,
    DwellTime = dwell:get_dwell_time(FirstDwell),

    % Convert to UTC (seconds precision).
    tgt_stats:date_ms_to_utc(MissTime, DwellTime).

%% Extract the sensor position from the dwell dict in standard units
%% (altitude converted to metres).
get_sensor_position_from_scan(#scan{grouped_dwells = GD}) ->

    % Use the position from the first dwell in the group.
    [FirstDwell|_Rest] = GD,

    SensorLat = dwell:get_sensor_lat(FirstDwell),
    SensorLon = dwell:get_sensor_lon(FirstDwell),
    SensorAlt = dwell:get_sensor_alt(FirstDwell),

    %% Convert parameters to the appopriate units for calculation.
    SensorAltMetres = cm_to_m(SensorAlt),

    {SensorLat, SensorLon, SensorAltMetres}.

%% Convert the sensor location to a form suitable for GeoJSON encoding.
sensor_to_geojson({Lat, Lon, Alt}, Timestamp, TimeUtc) ->
    LonN = coord:signed_lon(Lon),
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"time">>, list_to_binary(Timestamp)},
                        {<<"start">>, TimeUtc},
                        {<<"end">>, TimeUtc},
                        {<<"type">>, <<"Sensor">>}]},
     {<<"geometry">>, [{<<"type">>, <<"Point">>},
                       {<<"coordinates">>, [LonN, Lat, Alt]}]}].

%% Extract a flattened list of targets from the group of dwells comprising
%% a scan.
get_targets_from_scan(#scan{grouped_dwells = GD}) ->
    NestedTgts = lists:map(fun dwell:get_targets/1, GD),
    lists:flatten(NestedTgts).

%% Extract the relevant fields from a target report and produce a form suitable
%% suitable for GeoJSON encoding.
target_to_geojson(TgtRep, TimeStr, TimeUtc) ->
    HrLat = tgt_report:get_target_hr_lat(TgtRep),
    HrLon = tgt_report:get_target_hr_lon(TgtRep),
    HrLonN = coord:signed_lon(HrLon),
    Height = tgt_report:get_geodetic_height(TgtRep),
    gen_tgt_geojson(TimeStr, TimeUtc, HrLat, HrLonN, Height).

%% Convert the dwell area  parameters to a form suitable for GeoJSON encoding.
scan_polygon_to_geojson(TimeStr, TimeUtc, PolyPts) ->
    % We need to duplicate the first point at the end of the list.
    [Fst|_] = PolyPts,
    Reversed = [Fst|lists:reverse(PolyPts)],
    PolyPts2 = lists:reverse(Reversed),

    % Convert tuples to the GeoJSON Lon, Lat list form.
    GeoPts = lists:map(fun latlon_tuple_to_lonlat_list/1, PolyPts2),

    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"time">>, list_to_binary(TimeStr)},
                        {<<"start">>, TimeUtc},
                        {<<"end">>, TimeUtc}]},
     {<<"geometry">>, [{<<"type">>, <<"Polygon">>},
                       {<<"coordinates">>, [GeoPts]}]}].

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

%% Extract the dwell area parameters from the segment and convert to standard
%% units. Should be combined with the above function in due course (or one 
%% eliminated).
get_dwell_area_from_seg(DwellSeg) ->
    % Extract the parameters related to the dwell area from the segment.
    CentreLat = dwell:get_dwell_center_lat(DwellSeg),
    CentreLon = dwell:get_dwell_center_lon(DwellSeg),
    RangeHalfExtent = dwell:get_dwell_range_half_extent(DwellSeg),
    AngleHalfExtent = dwell:get_dwell_angle_half_extent(DwellSeg),

    %% Convert parameters to the appopriate units for calculation.
    RangeHalfExtentMetres = km_to_m(RangeHalfExtent),

    {CentreLat, CentreLon, RangeHalfExtentMetres, AngleHalfExtent}.

%% Extract the sensor position from the dwell segment in standard units
%% (altitude converted to metres).
get_sensor_position_from_seg(DwellSeg) ->
    SensorLat = dwell:get_sensor_lat(DwellSeg), 
    SensorLon = dwell:get_sensor_lon(DwellSeg),
    SensorAlt = dwell:get_sensor_alt(DwellSeg),

    %% Convert parameters to the appopriate units for calculation.
    SensorAltMetres = cm_to_m(SensorAlt),

    {SensorLat, SensorLon, SensorAltMetres}.

%% Convert distance in kilometres to metres.
km_to_m(Dist) -> Dist * 1000.

%% Convert a distance in centimetres to metres.
cm_to_m(Dist) -> Dist / 100.

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

%% Convert the target fields to a form suitable for GeoJSON encoding.
gen_tgt_geojson(Timestamp, TimeUtc, Lat, Lon, Alt) ->
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"time">>, list_to_binary(Timestamp)},
                        {<<"start">>, TimeUtc},
                        {<<"end">>, TimeUtc},
                        {<<"type">>, <<"Target">>}]},
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

%% Function to extract relevant points for a grouped dwell and form a bounding
%% polygon. Maps a list of dwell segments and creates a polygon by fusing the 
%% dwell areas into a single polygon.
grouped_dwells_to_polygon(GroupedDwells) ->
    Polys = lists:map(fun dwell_seg_to_polygon/1, GroupedDwells),
    fuse_polygons(Polys).

%% Create a dwell area polygon from a dwell segment.
dwell_seg_to_polygon(DwellSeg) ->
    DwellArea = get_dwell_area_from_seg(DwellSeg),
    SensorPos = get_sensor_position_from_seg(DwellSeg),
    dwell_area_to_polygon(DwellArea, SensorPos).

%% Third alternative which calculates a convex hull of the set of points.
fuse_polygons(Polys) when is_list(Polys) -> 
    % Convert the list of tuples to a list of lists.
    L1 = lists:map(fun tuple_to_list/1, Polys),
    % Flatten the list. 
    L2 = lists:flatten(L1),
    
    % Convert all of the points to a single ENU frame (keeping track of 
    % original coordinates).
    ENUref = lla_to_enu_with_ref(L2),
    
    % Compute the convex hull in the 2D ENU frame.
    Hull = convex_hull:quickhull2(ENUref),

    % Extract the reference (Lat,Lon,Alt) fields from each point.
    [Ref || {_E, _N, Ref} <- Hull]. 

%% Convert a list of Lat,Lon,Alt points to a list of ENU X,Y tuples with the
%% original LLA coordinates tagged on the end as the reference. This removes
%% the need for a reverse conversion later.
lla_to_enu_with_ref(Points) when is_list(Points) ->
    [{FLat,FLon}|_] = Points,
    FirstLLA = {FLat,FLon,0},
    F = fun(LatLon) -> 
            {Lat,Lon} = LatLon, 
            LLA = {Lat, Lon, 0},
            ECEF = coord:lla_to_ecef(LLA),
            {E, N, _} = coord:ecef_to_enu(FirstLLA, ECEF),
            {E, N, LatLon}
        end,
    lists:map(F, Points).

%% @doc Get the last mission segment from a location update.
-spec get_last_mission(loc_update()) -> mission:mission_segment().
get_last_mission(#loc_update{last_mission  = X}) -> X.

%% @doc Get the location data from the update.
-spec get_loc_data(loc_update()) -> platform_loc:platform_location_segment().
get_loc_data(#loc_update{loc_data = X}) -> X.

%% @doc Get the list of grouped dwells in a scan.
-spec get_grouped_dwells(scan()) -> [dwell:dwell_segment()].
get_grouped_dwells(#scan{grouped_dwells = X}) -> X.

