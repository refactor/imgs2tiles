-module(gdal_nifs).

-export([init/0]).
-export([open/1,
        close/1]).
-export([get_meta/1]).

-on_load(init/0).

init() ->
    erlang:load_nif("./gdal_nifs", 0).

open(Filename) ->
    case open_img(Filename) of
        {ok, Hdataset} ->
            ok = calc_nodatavalue(Hdataset),
            ok = calc_srs(Hdataset),
            ok = warp_dataset(Hdataset),
            {ok, Hdataset};
        {error, _} = Err ->
            Err
    end.

close(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

get_meta(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

%% ---------------------------------------------------
%% private nif function
%% ---------------------------------------------------
open_img(Filename) ->
    erlang:error(function_clause, ["NIF library not loaded",Filename]).

calc_nodatavalue(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

calc_srs(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

warp_dataset(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).
