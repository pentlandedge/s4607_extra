-module(tgt_filter).

-export([
    filter_dwell_targets/2, 
    filter_targets/2, 
    box_pred/3,
    create_target_pred/2]).

%% Clone of dwell segment record definition. Can be removed when the update 
%% function has been implemented properly
-record(dwell_segment, {
    existence_mask,
    revisit_index,
    dwell_index,
    last_dwell_of_revisit,
    target_report_count,
    dwell_time,
    sensor_lat,
    sensor_lon,
    sensor_alt,
    lat_scale_factor,
    lon_scale_factor,
    spu_along_track,
    spu_cross_track,
    spu_alt,
    sensor_track,
    sensor_speed,
    sensor_vert_vel,
    sensor_track_unc,
    sensor_speed_unc,
    sensor_vert_vel_unc,
    platform_heading,
    platform_pitch,
    platform_roll,
    dwell_center_lat,
    dwell_center_lon,
    dwell_range_half_extent,
    dwell_angle_half_extent,
    sensor_heading,
    sensor_pitch,
    sensor_roll,
    mdv,
    targets}).

%% Appies the predicate to the targets in the dwell segment and constructs a
%% new dwell segment.
filter_dwell_targets(Pred, DwellSegData) ->
    Targets = dwell:get_targets(DwellSegData),
    NewTargets = filter_targets(Pred, Targets),
    update_targets(DwellSegData, NewTargets).

%% Applies the supplied predicate to the list of targets.
filter_targets(Pred, Targets) when is_function(Pred), is_list(Targets) ->
    lists:filter(Pred, Targets).

%% Update the targets in a dwell segment. This is a bit of a hack: should 
%% really provide a way of doing this in the main s4607 libarary.
update_targets(DwellSegData, NewTargets) ->
    TgtCount = length(NewTargets),
    DwellSegData#dwell_segment{
        targets = NewTargets,  
        target_report_count = TgtCount}.

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

