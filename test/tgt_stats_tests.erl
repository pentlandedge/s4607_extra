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
    [datetime_string_checks(), extract_check1(), geojson_check1(),
     dwell_area_to_polygon_checks(), jd_bounding_area_checks(), 
     jd_from_packet_list_checks()].

datetime_string_checks() ->
    DT = {{2016,7,27},{8,12,59}},
    TimeStr = tgt_stats:datetime_to_string(DT),
    MS = (((8*60 + 12)*60) + 59) * 1000,
    TimeUtc = tgt_stats:date_ms_to_utc({2016,7,27},MS),

    [?_assertEqual("2016-07-27 08:12:59", TimeStr),
     ?_assertEqual(1469607179000, TimeUtc)].

extract_check1() ->
    PacketList = packet_list:get_list1(),
    TgtStats = tgt_stats:extract(PacketList),

    % Expect only a single dictionary to be returned.
    [DwellDict] = TgtStats,

    % Extract the mission time.
    MissTime = dict:fetch(mission_time, DwellDict),

    % Extract the dwell time.
    DwellTime = dict:fetch(dwell_time, DwellDict),

    % Convert to UTC (seconds precision).
    DwellUTC = tgt_stats:date_ms_to_datetime(MissTime, DwellTime),

    [?_assertEqual({2016, 7, 28}, MissTime),
     ?_assertEqual({{2016, 7, 28},{0, 16, 40}}, DwellUTC)].

geojson_check1() ->
    PacketList = packet_list:get_list1(),
    TgtStats = tgt_stats:extract(PacketList),

    % Expect only a single dictionary to be returned.
    [DwellDict] = TgtStats,

    % Convert it to GeoJSON format.
    GJ = tgt_stats:dwell_to_geojson(DwellDict),

    % Extract the first part of the GeoJSON containing type.
    <<TypeStr:27/binary,Text2:52/binary,TimeStr:21/binary,_Rest/binary>> = GJ,

    % Next bit of the file should look like this.
    F2 = <<",\"features\":[{\"type\":\"Feature\",\"properties\":{\"time\":">>,

    % This is the expected dwell time.
    DwellTime = <<"\"2016-07-28 00:16:40\"">>,

    [?_assertEqual(<<"{\"type\":\"FeatureCollection\"">>, TypeStr),
     ?_assertEqual(F2, Text2),
     ?_assertEqual(DwellTime, TimeStr)].

dwell_area_to_polygon_checks() ->
    % 5km range swathe.
    RangeHalfExtentKM = 2.5,
    DwellRangeHalfExtentM = RangeHalfExtentKM * 1000,
    % Dwell centre on the East Fortune runway crossing.
    % Use a 45 degree width of the dwell.
    DwellArea = {55.999591, -2.718204, DwellRangeHalfExtentM, 22.5},

    % Dwell altitute is expressed in cm above WGS84 ellipsoid. Convert to
    % metres before usage.
    % Use the Garvald Inn as our sensor position, at an altitude of 1000m.
    DwellAltCm = 100000,
    DellAltM = DwellAltCm / 100.0,
    SensorPos = {55.928613, -2.66116, DellAltM},

    {PtA, PtB, PtC, PtD} = tgt_stats:dwell_area_to_polygon(DwellArea, SensorPos),
    % Distance from sensor to dwell centre is 8654m, initial bearing 335.803889 deg.
    % Use the online
    {LatA, LonA} = PtA,
    {LatB, LonB} = PtB,
    {LatC, LonC} = PtC,
    {LatD, LonD} = PtD,
    [?_assert(almost_equal(55.966667, LatA, 0.001)),
     ?_assert(almost_equal(-2.733056, LonA, 0.001)),
     ?_assert(almost_equal(55.997222, LatB, 0.001)),
     ?_assert(almost_equal(-2.791667, LonB, 0.001)),
     ?_assert(almost_equal(56.028889, LatC, 0.001)),
     ?_assert(almost_equal(-2.666389, LonC, 0.001)),
     ?_assert(almost_equal(55.983889, LatD, 0.001)),
     ?_assert(almost_equal(-2.664167, LonD, 0.001))].

%% Test the extraction of the job definition bounding area.
jd_bounding_area_checks() ->
    JD = sample_job_def(),
    Bound = tgt_stats:get_bounding_area(JD),
    {{LatA, LonA},{LatB, LonB},{LatC, LonC},{LatD, LonD}} = Bound,
    [?_assert(almost_equal(33.3,  LatA, 0.001)),
     ?_assert(almost_equal(3.45,  LonA, 0.001)),
     ?_assert(almost_equal(23.4,  LatB, 0.001)),
     ?_assert(almost_equal(350.0, LonB, 0.001)),
     ?_assert(almost_equal(-45.0, LatC, 0.001)),
     ?_assert(almost_equal(2.45,  LonC, 0.001)),
     ?_assert(almost_equal(-60.0, LatD, 0.001)),
     ?_assert(almost_equal(140.0, LonD, 0.001))].

% Extract the job definition segment from a packet list check.
jd_from_packet_list_checks() ->
    PackList = packet_list:get_list2(),
    Segs = s4607:get_segments_by_type([job_definition], PackList),
    [JobDefSeg] = Segs,
    SH = segment:get_header(JobDefSeg),
    JD = segment:get_data(JobDefSeg),
    T = seg_header:get_segment_type(SH),
    Bound = tgt_stats:get_bounding_area(JD),
    {{LatA, LonA},{LatB, LonB},{LatC, LonC},{LatD, LonD}} = Bound,
    [?_assertEqual(T, job_definition),
     ?_assert(almost_equal(33.3,  LatA, 0.001)),
     ?_assert(almost_equal(3.45,  LonA, 0.001)),
     ?_assert(almost_equal(23.4,  LatB, 0.001)),
     ?_assert(almost_equal(350.0, LonB, 0.001)),
     ?_assert(almost_equal(-45.0, LatC, 0.001)),
     ?_assert(almost_equal(2.45,  LonC, 0.001)),
     ?_assert(almost_equal(-60.0, LatD, 0.001)),
     ?_assert(almost_equal(140.0, LonD, 0.001))].

%% Utility function to compare whether floating point values are within a
%% specified range.
almost_equal(V1, V2, Delta) ->
    abs(V1 - V2) =< Delta.

%% Create a sample job definition segment for testing.
sample_job_def() ->
    job_def:new(sample_job_def_params()).

sample_job_def_params() ->
    [{job_id, 100}, {sensor_id_type, rotary_wing_radar},
     {sensor_id_model, "Heli 1"}, {target_filt_flag, no_filtering}, {priority, 30},
     {bounding_a_lat, 33.3}, {bounding_a_lon, 3.45},
     {bounding_b_lat, 23.4}, {bounding_b_lon, 350},
     {bounding_c_lat, -45.0}, {bounding_c_lon, 2.45},
     {bounding_d_lat, -60.0}, {bounding_d_lon, 140},
     {radar_mode, {monopulse_calibration, asars_aip}}, {nom_rev_int, 65000},
     {ns_pos_unc_along_track, no_statement},
     {ns_pos_unc_cross_track, 5000}, {ns_pos_unc_alt, 20000},
     {ns_pos_unc_heading, 45}, {ns_pos_unc_sensor_speed, 65534},
     {ns_val_slant_range_std_dev, 100},
     {ns_val_cross_range_std_dev, no_statement},
     {ns_val_tgt_vel_los_std_dev, 4000}, {ns_val_mdv, no_statement},
     {ns_val_det_prob, 100}, {ns_val_false_alarm_density, 254},
     {terr_elev_model, dgm50}, {geoid_model, geo96}].

