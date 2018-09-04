%% Simple command line argument processing.
-module(cmd_args).

-export([collect/1, collect/5, all_present/2]).
-export([opt_map/1]).

-export([read_box/1]).

%% Collect arguments associated with each option found in the list.
collect(Args) ->
    collect(Args, [], [], undefined, []).

collect([], AccProps, AccNoOpt, undefined, _) ->
    {AccProps, AccNoOpt};
collect([], AccProps, AccNoOpt, CurrOpt, CurrArgs) ->
    % Add the current option and arguments to the list and return
    NewProps = [{CurrOpt, CurrArgs}|AccProps],
    {NewProps, AccNoOpt};
collect([Token|Rest], AccProps, AccNoOpt, undefined, CurrArgs) ->
    case opt_map(Token) of
        undefined ->
            collect(Rest, AccProps, [Token|AccNoOpt], undefined, CurrArgs);
        OptAtom   ->
            collect(Rest, AccProps, AccNoOpt, OptAtom, [])
    end;
collect([Token|Rest], AccProps, AccNoOpt, CurrOpt, CurrArgs) ->
    case opt_map(Token) of
        undefined ->
            collect(Rest, AccProps, AccNoOpt, CurrOpt, lists:append(CurrArgs, [Token]));
        OptAtom   ->
            NewProps = [{CurrOpt, CurrArgs}|AccProps],
            collect(Rest, NewProps, AccNoOpt, OptAtom, [])
    end.

opt_map("--file")       -> file;   
opt_map("--output")     -> output;   
opt_map("--box")        -> box;   
opt_map("--time")       -> time;   
opt_map(_)              -> undefined.   

string_to_number(NumStr) ->
    case string:to_float(NumStr) of 
        {error, no_float} -> {X, _} = string:to_integer(NumStr), X; 
        {X, _}            -> X 
    end. 

all_present(Keys, Proplist) ->
    Missing = [X || X <- Keys, proplists:is_defined(X, Proplist) == false],
    case Missing of
        [] -> true;
        _  -> {false, Missing}
    end.

read_box([BL_LatS, BL_LonS, TR_LatS, TR_LonS]) -> 
    F = fun(NumStr) -> string_to_number(NumStr) end,
    {{F(BL_LatS),F(BL_LonS)},{F(TR_LatS),F(TR_LonS)}}.
