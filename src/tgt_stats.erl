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

-export([extract/1, dwell_dicts_to_geojson/1, dwell_to_geojson/1]).

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
    lists:map(fun dwell_to_geojson/1, DwellList).

%% Function to convert a single dwell dictionary structure to the GeoJSON
%% form.
dwell_to_geojson(_DwellDict) ->
    % Dummy/hardcoded parameters for now.
    TimeStr = "2014-09-10 09:42:26+01",
    DwellArea = {1,2,3,4},
    SensorPos = {1,2,3},
    {PtA, PtB, PtC, PtD} = dwell_area_to_polygon(DwellArea, SensorPos),
    
    DwellAreaGeo = dwell_area_to_geojson(TimeStr, PtA, PtB, PtC, PtD),
    Tgt1 = gen_tgt_geojson("2014-09-10 09:42:26+01", 55.9987, -2.71, 1),            
    Tgt2 = gen_tgt_geojson("2014-09-10 09:43:26+01", 55.9988, -2.711, 1),

    jsx:encode([
        {<<"type">>,<<"FeatureCollection">>}, 
        {<<"features">>, [DwellAreaGeo, Tgt1, Tgt2]}
    ]). 

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
dwell_area_to_polygon(_DwellArea, _SensorPos) ->
    % Hardcode these at present. Note they are in Lon, Lat pairs.
    PtA = [-2.735, 55.985],
    PtB = [-2.74, 56.015],
    PtC = [-2.675, 56.015],
    PtD = [-2.68, 55.985],
    {PtA, PtB, PtC, PtD}.

gen_tgt_geojson(Timestamp, Lat, Lon, Alt) ->
    [{<<"type">>, <<"Feature">>},
     {<<"properties">>, [{<<"time">>, list_to_binary(Timestamp)}]},
     {<<"geometry">>, [{<<"type">>, <<"Point">>},
                       {<<"coordinates">>, [Lon, Lat, Alt]}]}
    ]. 

