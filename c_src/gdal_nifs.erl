-module(gdal_nifs).

-export([init/0]).
-export([open/1,
        close/1]).
-export([get_meta/1]).
-export([get_bound/1, get_pixelsize/1, get_rastersize/1]).
-export([calc_zoomlevel_range/1, calc_swne/1, calc_tminmax/1]).

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
%% private function
%% ---------------------------------------------------

calc_tminmax(Ref) ->
    {ok, Bound} = get_bound(Ref),
    calc_tminmax(Bound, [], 0).

calc_tminmax(_Bound, Tminmax, 32) ->
    lists:reverse(Tminmax);
calc_tminmax({Ominx, Ominy, Omaxx, Omaxy} = Bound, Tminmax, Zoom) ->
    {Tminx, Tminy} = mercator_tiles:meters_to_tile( Ominx, Ominy, Zoom ),
    {Tmaxx, Tmaxy} = mercator_tiles:meters_to_tile( Omaxx, Omaxy, Zoom ),
    Z = trunc(math:pow(2, Zoom)) - 1,
    BoundOfZoom = {
        max(0, Tminx), max(0, Tminy), 
        min(Z, Tmaxx), min(Z, Tmaxy)
    },
    calc_tminmax(Bound, [BoundOfZoom|Tminmax], Zoom + 1).

    
%% @doc Get the minimal and maximal zoom level
%% minimal zoom level: map covers area equivalent to one tile
%% maximal zoom level: closest possible zoom level up on the resolution of raster
calc_zoomlevel_range(Ref) ->
    {ok, {RasterXSize, RasterYSize}} = get_rastersize(Ref),
    {ok, {PixelXSize, _PixelYSize}} = get_pixelsize(Ref),
    Tminz = mercator_tiles:zoom_for_pixelsize( PixelXSize * max( RasterXSize, RasterYSize) / 256 ),
    Tmaxz = mercator_tiles:zoom_for_pixelsize( PixelXSize ),
    {ok, {Tminz, Tmaxz}}.

calc_swne(Ref) ->
    {Ominx, Ominy, Omaxx, Omaxy} = get_bound(Ref),
    {South, West} = mercator_tiles:meters_to_latlon(Ominx, Ominy),
    {North, East} = mercator_tiles:meters_to_latlon(Omaxx, Omaxy),
    {ok, {max(-85.05112878, South), max(-180.0, West), min(85.05112878, North), min(180.0, East)}}.

%% ---------------------------------------------------
%% private nif function
%% ---------------------------------------------------

%% @doc Bounds in meters
get_bound(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

get_pixelsize(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

get_rastersize(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

open_img(Filename) ->
    erlang:error(function_clause, ["NIF library not loaded",Filename]).

calc_nodatavalue(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

calc_srs(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

warp_dataset(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).
