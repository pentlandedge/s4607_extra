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

-export([extract/1]).

-record(stat_acc, {ref_time, dwell_list}).

%% Function to extract the most useful fields relating to targets present in
%% a list of decoded s4607 packets.
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
                    % Extract all of the parameters of interest.
                    KV1 = {mission_time, RT},
                    KV2 = {target_report_count, dwell:get_target_report_count(SegData)},
                    KV3 = {target_report_count, dwell:get_target_report_count(SegData)},
                    KV4 = {sensor_lat, dwell:get_sensor_lat(SegData)},
                    KV5 = {sensor_lon, dwell:get_sensor_lon(SegData)},
                    KV6 = {sensor_alt, dwell:get_sensor_alt(SegData)},
                    KV7 = {dwell_center_lat, dwell:get_dwell_center_lat(SegData)}, 
                    KV8 = {dwell_center_lon, dwell:get_dwell_center_lon(SegData)}, 
                    KV9 = {dwell_range_he, dwell:get_dwell_range_half_extent(SegData)}, 
                    KV10 = {dwell_angle_he, dwell:get_dwell_angle_half_extent(SegData)},

                    % Create a key/value list of the required parameters.
                    Props = [KV1, KV2, KV3, KV4, KV5, KV6, KV7, KV8, KV9, KV10],

                    % Check to see if there are any target reports.
                    TRC = dwell:get_target_report_count(SegData),
                    EM = dwell:get_existence_mask(SegData),

                    % If there are any targets then convert each to a dict.
                    Props2 = case TRC of
                        0 ->
                            Props;
                        _ -> 
                            Tgts = dwell:get_targets(SegData),
                            F = fun(TR) ->
                                    tgt_report:to_dict(TR, EM)
                                end,
                            TgtsVal = lists:map(F, Tgts),
                            [{targets, TgtsVal}|Props]
                    end,
                            
                    % Turn it into a dictionary.
                    DwellParams = dict:from_list(Props2),

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

