-module(tgt_filter).

-export([
    filter_dwells_in_packetlist/2,
    filter_targets_in_packetlist/2,
    filter_dwell_targets/2, 
    filter_targets/2, 
    box_pred/3,
    create_target_pred/2,
    create_dwell_time_pred/2,
    dwell_offset_to_time/1,
    dwell_offset_to_time_ms/1]).

%% Applies the predicate function to the dwells in the list of packets.
filter_dwells_in_packetlist(Pred, PacketList) when is_function(Pred), 
    is_list(PacketList) ->
    F = fun(Packet, {RefDate, AccPktList}) ->
            %filter_dwells_in_packet(Pred, Packet)
            case update_packet(Pred, Packet, RefDate) of
                {ok, Pkt, NewRefDate} ->
                    {NewRefDate, [Pkt|AccPktList]};
                drop ->
                    {RefDate, AccPktList}
            end
        end,
    {_, ReversedList} = lists:foldl(F, {{1970,1,1},[]}, PacketList),
    lists:reverse(ReversedList).

%% Function to update a packet. If the filtered list of segments is empty
%% then the packet is dropped.
update_packet(Pred, Packet, RefDate) ->
    Segs = s4607:get_packet_segments(Packet),
    {NewRefDate, ReversedSegs} = lists:foldl(Pred, {RefDate, []}, Segs),
    case ReversedSegs of
        [] -> 
            drop;
        _  -> 
            NewSegs = lists:reverse(ReversedSegs),
            NewPacket = s4607:update_segments_in_packet(Packet, NewSegs),
            {ok, NewPacket, NewRefDate}
    end.

%% Function to create a time based predicate that can be used to filter dwell 
%% segments using a fold. Since the mission time can be updated during the 
%% sequence, this is threaded through the accumulator.
create_dwell_time_pred(StartTime, EndTime) ->
    fun(Seg, {RefDate, SegList}) ->
        SegHdr = segment:get_header(Seg),
        SegType = seg_header:get_segment_type(SegHdr), 
        SegData = segment:get_data(Seg),

        case SegType of 
            dwell ->
                DwellTimeMS = dwell:get_dwell_time(SegData),
                DwellTime = dwell_offset_to_time(DwellTimeMS),
                DateTime = {RefDate, DwellTime},
                case datetime_in_interval(DateTime, StartTime, EndTime) of
                    true ->
                        {RefDate, [Seg|SegList]};
                    false ->
                        {RefDate, SegList}
                end;
            mission ->
                % Update the reference date when a mission segment is received.
                MissionDate = mission:get_time(SegData),
                {MissionDate, [Seg|SegList]};
            _ ->
                % Accumulate other segments unmodified.
                {RefDate, [Seg|SegList]}
        end
    end.

%% Function to test whether a datetime() lies within the specified interval
datetime_in_interval(DateTime, Start, End) ->
    % Conversions to Gregorian seconds probably unnecessary.
    GregSec = calendar:datetime_to_gregorian_seconds(DateTime),
    StartSec = calendar:datetime_to_gregorian_seconds(Start),
    EndSec = calendar:datetime_to_gregorian_seconds(End),
    (GregSec >= StartSec) and (GregSec =< EndSec).

%% Function to convert a dwell offset in ms to a time (truncating to 
%% seconds).
dwell_offset_to_time(DwellMS) ->
    Secs = trunc(DwellMS / 1000),
    calendar:seconds_to_time(Secs).

%% Function to convert a dwell offset in ms to a time including the ms field.
dwell_offset_to_time_ms(DwellMS) ->
    Secs = DwellMS div 1000,
    MS = DwellMS rem 1000, 
    {H,M,S} = calendar:seconds_to_time(Secs),
    {H,M,S,MS}.

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
        box_pred({TgtLat,-360.0 + TgtLon},TL, BR)
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

