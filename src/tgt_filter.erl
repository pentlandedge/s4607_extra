-module(tgt_filter).

-export([
    filter_dwells_in_packetlist/2,
    filter_targets_in_packetlist/2,
    filter_dwell_targets/2, 
    filter_targets/2, 
    box_pred/3,
    create_target_pred/2]).

-record(dwell_datetime, {mission_ref, dwell_ms}).

%% Applies the predicate function to the dwells in the list of packets.
filter_dwells_in_packetlist(Pred, PacketList) when is_function(Pred), 
    is_list(PacketList) ->
    F = fun(Packet, {RefDate, AccPktList}) ->
            %filter_dwells_in_packet(Pred, Packet)
            {RefDate, [Packet|AccPktList]}
        end,
    {_, ReversedList} = lists:foldl(F, {{1970,1,1},[]}, PacketList),
    lists:reverse(ReversedList).

%% Applies the predicate function to the targets in the list of packets.
filter_targets_in_packetlist(Pred, PacketList) when is_function(Pred), 
    is_list(PacketList) ->
    F = fun(Packet) ->
            filter_targets_in_packet(Pred, Packet)
        end,
    lists:map(F, PacketList).

%% Filters targets in a single packet.
filter_targets_in_packet(Pred, Packet) when is_function(Pred) ->
    Segs = s4607:get_packet_segments(Packet),
    F = fun(Seg) ->
            SegHdr = segment:get_header(Seg),
            SegType = seg_header:get_segment_type(SegHdr), 
            case SegType of 
                dwell ->
                    DwellData = segment:get_data(Seg),
                    NewDwellData = filter_dwell_targets(Pred, DwellData),
                    segment:update_segment_data(Seg, NewDwellData);
                _     ->
                    Seg
            end
        end,
    NewSegs = lists:map(F, Segs), 
    s4607:update_segments_in_packet(Packet, NewSegs).

%% Appies the predicate to the targets in the dwell segment and constructs a
%% new dwell segment.
filter_dwell_targets(Pred, DwellSegData) ->
    Targets = dwell:get_targets(DwellSegData),
    NewTargets = filter_targets(Pred, Targets),
    dwell:update_targets(DwellSegData, NewTargets).

%% Applies the supplied predicate to the list of targets.
filter_targets(Pred, Targets) when is_function(Pred), is_list(Targets) ->
    lists:filter(Pred, Targets).

%% Create a target box filtering predicate function.
create_target_pred({TL_Lat, TL_Lon} = TL, {BR_Lat, BR_Lon} = BR) when 
    TL_Lat >= BR_Lat, TL_Lon =< BR_Lon ->

    fun(Tgt) ->
        TgtLat = tgt_report:get_target_hr_lat(Tgt),
        TgtLon = tgt_report:get_target_hr_lon(Tgt),
        box_pred({TgtLat,TgtLon},TL, BR)
    end.

%% Predicate function to filter a Lat, Lon point depending upon whether it 
%% falls within a box defined by a top left and bottom right box.
%% As it stands, this won't work at the poles or dateline.
box_pred({Lat, Lon}, {TL_Lat, TL_Lon}, {BR_Lat, BR_Lon}) when
    Lat =< TL_Lat, Lat >= BR_Lat, Lon >= TL_Lon, Lon =< BR_Lon -> 
    true;
box_pred({_, _}, {TL_Lat, TL_Lon}, {BR_Lat, BR_Lon}) when
    TL_Lat >= BR_Lat, TL_Lon =< BR_Lon ->
    false. 

%%data_time_pred({Y,M,D} = MissRef, DwellMS, 

%% Comparison function for dwell times referenced to a mission segment date.
%% Follows C library qsort comparison function logic.
%% Compares dates first, looks at times if dates are equal.
%compare_dwell_datetime(
%    #dwell_datetime{mission_ref = MR1}, 
%    #dwell_datetime{mission_ref = MR2}) when MR1 < MR2 -> -1;
%compare_dwell_datetime(
%    #dwell_datetime{mission_ref = MR1}, 
%    #dwell_datetime{mission_ref = MR2}) when MR1 > MR2 -> 1;
%compare_dwell_datetime(
%    #dwell_datetime{dwell_ms = DT1}, 
%    #dwell_datetime{dwell_ms = DT2}) when DT1 < DT2 -> -1;
%compare_dwell_datetime(
%    #dwell_datetime{dwell_ms = DT1}, 
%    #dwell_datetime{dwell_ms = DT2}) when DT1 > DT2 -> 1;
%compare_dwell_datetime(
%    #dwell_datetime{}, 
%    #dwell_datetime{}) -> 0.

