%% Routines to assist with the replay of old trials data as if it were 
%% happening live. 

-module(s4607_replay).

-export([
    init_replay_state/1,
    update_packet/3, 
    map_packet/5, 
    patch_mission_seg_data/2, 
    patch_dwell_seg_data/2]).

-record(replay, {mission_date, dwell_offset}).

%% @doc Initialise a replay state structure.
init_replay_state(MissionDate) ->
    #replay{mission_date = MissionDate, dwell_offset = undefined}.

%% @doc Update any mission and dwell segments in the packet to the new date
%% and time. Returns a new packet and replay state.
update_packet(Packet, ReplayState, CurrentTimeMS) ->
    PH = s4607:get_packet_header(Packet),
    Segs = s4607:get_packet_segments(Packet),
    F = fun(Seg, {SegList, Replay}) ->
            {NewSeg, NewReplay} = patch_segment(Seg, Replay, CurrentTimeMS),
            NewSegList = [NewSeg|SegList],
            {NewSegList, NewReplay}
        end,
    {NewSegs, NewReplay} = lists:foldl(F, {[], ReplayState}, Segs),
    NewPacket = s4607:new_packet(PH, lists:reverse(NewSegs)),
    {NewPacket, NewReplay}.

%% @doc Patch a segment and update the replay state if required.
patch_segment(Seg, Replay, TimeMS) ->
    #replay{mission_date = MD, dwell_offset = Offset} = Replay,
    SH = segment:get_header(Seg),
    SegData = segment:get_data(Seg),
    Type = seg_header:get_segment_type(SH),
    case Type of
        mission ->
            NewReplay = Replay,
            NewSegData = patch_mission_seg_data(SegData, MD),
            NewSeg = segment:new0(SH, NewSegData);
        dwell ->
            case Offset of
                undefined ->
                    OrigDwellTime = dwell:get_dwell_time(SegData),
                    NewOffset = TimeMS - OrigDwellTime, 
                    NewReplay = #replay{dwell_offset = NewOffset}, 
                    NewSegData = patch_dwell_seg_data(SegData, TimeMS),
                    NewSeg = segment:new0(SH, NewSegData);
                _ ->
                    NewSeg = Seg,
                    NewReplay = Replay
            end;
        _ ->
            NewSeg = Seg,
            NewReplay = Replay
    end,
    {NewSeg, NewReplay}.

%% @doc Update any mission and dwell segments in the packet to new dates and
%% times. Return the updated packet and a note of the last dwell time, which
%% can be used to set the delay before transmitting the packet.
map_packet(Packet, MissionDate, FirstDwellRx, DwellOffset, CurrentDwellTime) ->
    PH = s4607:get_packet_header(Packet),
    Segs = s4607:get_packet_segments(Packet),
    F = fun(S, {LastDwellTime, SegList}) ->
            {DwellRx, DwOff, LastDwell, NewSeg} = patch_segment(S, MissionDate, FirstDwellRx, DwellOffset, CurrentDwellTime, LastDwellTime),
            {DwellRx, DwOff, LastDwell, [NewSeg|SegList]}
        end,
    {NewFirstDwellRx, NewDwellOffset, NewLastDwellTime, NewSegs} = lists:foldl(F, {no_dwells, []}, Segs),
    NewPacket = s4607:new_packet(PH, lists:reverse(NewSegs)),
    {NewPacket, NewFirstDwellRx, NewDwellOffset, NewLastDwellTime}.

patch_segment(Seg, MissionDate, FirstDwellRx, DwellOffset, CurrentDwellTime, LastDwellTime) ->
    SH = segment:get_header(Seg),
    SegData = segment:get_data(Seg),
    Type = seg_header:get_segment_type(SH),
    case Type of
        mission ->
            NewSegData = patch_mission_seg_data(SegData, MissionDate),
            io:format("Mission segment ~p~n", [MissionDate]),
            {FirstDwellRx, DwellOffset, LastDwellTime, segment:new0(SH, NewSegData)};
        dwell ->
            io:format("Dwell segment ~n"),
            OrigDwellTime = dwell:get_dwell_time(SegData),
            case FirstDwellRx of
                false ->
                    Adjusted = CurrentDwellTime,
                    NewDwellOffset = CurrentDwellTime - OrigDwellTime,
                    io:format("First: Orig ~p, Adj ~p, NewDO ~p~n", [OrigDwellTime, Adjusted, NewDwellOffset]),
                    NewFirstDwellRx = true;
                true ->
                    Adjusted = OrigDwellTime + DwellOffset,
                    NewDwellOffset = DwellOffset,
                    io:format("Not first: Orig ~p, Adj ~p, NewDO ~p~n", [OrigDwellTime, Adjusted, NewDwellOffset]),
                    NewFirstDwellRx = true
            end,
            NewSegData = patch_dwell_seg_data(SegData, Adjusted),
            io:format("Adjusted ~p~n", [Adjusted]),
            {NewFirstDwellRx, NewDwellOffset, Adjusted, segment:new0(SH, NewSegData)};
        _ ->
            io:format("Pass through ~p~n", [Type]),
            {FirstDwellRx, DwellOffset, LastDwellTime, Seg}
    end.

%% @doc Update the reference date in a mission segment.
patch_mission_seg_data(MS, Date) ->
    mission:set_date(MS, Date).

%% @doc Update the dwell time in a segment.
patch_dwell_seg_data(DS, DwellTimeMS) ->
    dwell:set_dwell_time(DS, DwellTimeMS).

%% @doc Update the last dwell time
update_last_dwell_time(no_dwells, DTnew) -> 
    DTnew;
update_last_dwell_time(DTold, DTnew) when DTnew > DTold -> 
    DTnew;
update_last_dwell_time(DTold, _) -> 
    DTold.

