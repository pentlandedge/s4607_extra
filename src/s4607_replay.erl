%% Routines to assist with the replay of old trials data as if it were 
%% happening live. 

-module(s4607_replay).

-export([map_packet/3]).

%% @doc Update any mission and dwell segments in the packet to new dates and
%% times. Return the updated packet and a note of the last dwell time, which
%% can be used to set the delay before transmitting the packet.
map_packet(Packet, _MissionDate, _DwellOffset) ->
    LastDwellTime = 0,
    {Packet, LastDwellTime}.
