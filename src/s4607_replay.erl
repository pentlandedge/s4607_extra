%% Routines to assist with the replay of old trials data as if it were 
%% happening live. 

-module(s4607_replay).

-export([map_packet/3, patch_mission_seg/2]).

%% @doc Update any mission and dwell segments in the packet to new dates and
%% times. Return the updated packet and a note of the last dwell time, which
%% can be used to set the delay before transmitting the packet.
map_packet(Packet, MissionDate, DwellOffset) ->
    PH = s4607:get_packet_header(Packet),
    Segs = s4607:get_packet_segments(Packet),
    F = fun(S, Acc) ->
            NewSeg = patch_segment(S, MissionDate, DwellOffset),
            [NewSeg|Acc]
        end,
    NewSegs = lists:foldl(F, [], Segs),
    LastDwellTime = 0,
    NewPacket = s4607:new_packet(PH, lists:reverse(NewSegs)),
    {NewPacket, LastDwellTime}.

patch_segment(Seg, MissionDate, DwellOffset) ->
    SH = segment:get_header(Seg),
    SegData = segment:get_data(Seg),
    Type = seg_header:get_segment_type(SH),
    case Type of
        mission ->
            NewSegData = patch_mission_seg(SegData, MissionDate),
            segment:new0(SH, NewSegData);
        dwell ->
            io:format("Dwell segment ~n"),
            DwellTimeMS = dwell:get_dwell_time(SegData) + DwellOffset,
            NewSegData = patch_dwell_seg(SegData, DwellTimeMS),
            segment:new0(SH, NewSegData);
        _ ->
            Seg
    end.

%% @doc Update the reference date in a mission segment.
patch_mission_seg(MS, Date) ->
    mission:set_date(MS, Date).

%% @doc Update the dwell time in a segment.
patch_dwell_seg(DS, DwellTimeMS) ->
    dwell:set_dwell_time(DS, DwellTimeMS).
