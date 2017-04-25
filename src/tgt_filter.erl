-module(tgt_filter).

-export([
    filter_dwell_targets/2, 
    filter_targets/2, 
    box_pred/3,
    create_target_pred/2]).

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

