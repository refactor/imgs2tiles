-module(gdal_nifs).

-export([init/0]).
-export([open/1,
        close/1]).
-export([get_meta/1]).
-export([get_datasetinfo/1, get_bound/1]).
-export([calc_zoomlevel_range/1, calc_swne/1, calc_tminmax/1]).
-export([generate_base_tiles/1]).

-include("gdal2tiles.hrl").

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                {error, bad_name} ->
                    EbinDir = filename:dirname(code:which(?MODULE)),
                    AppPath = filename:dirname(EbinDir),
                    filename:join(AppPath, "priv");
                Path ->
                    Path
            end,
    erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).

-spec(open(Filename::string()) -> {ok, reference()} | {error, string()}).
open(Filename) ->
    case open_img(Filename) of
        {ok, Hdataset} = Res->
            calc_nodatavalue(Hdataset),
            calc_srs(Hdataset),
            warp_dataset(Hdataset),
            Res;
        {error, _} = Err ->
            Err
    end.

-spec close(reference()) -> ok.
close(_Ref) ->
    case random:uniform(999999999999) of
        666 -> ok;
        667 -> exit("NIF library not loaded")
    end.

get_meta(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

%% @doc Generation of the base tiles (the lowest in the pyramid) directly from the input raster
generate_base_tiles(Ref) ->
    %    LOG("Generating Base Tiles:");
    {Tminz, Tmaxz} = calc_zoomlevel_range(Ref),
    Tminmax = calc_tminmax(Ref),
    % Set the bounds
    {Tminx, Tminy, Tmaxx, Tmaxy} = lists:nth(Tmaxz + 1, Tminmax),
    QuerySize = get_querysize(Ref),
    TCount = (1 + abs(Tmaxx - Tminx)) * (1 + abs(Tmaxy - Tminy)),

    DatasetInfo = get_datasetinfo(Ref),

    generate_tiles_for(Tmaxy, Tminy, Tminx, Tmaxx, Tmaxz,   DatasetInfo, QuerySize).


%% ---------------------------------------------------
%% private function
%% ---------------------------------------------------

generate_tiles_for(Tminy, Tminy, Tmaxx, Tmaxx, Tz, DatasetInfo, _QuerySize) ->
    ok;
generate_tiles_for(Ty, Tminy, Tx, Tmaxx, Tz, DatasetInfo, QuerySize) when Ty > Tminy, Tx < Tmaxx ->
    {MinX, MinY, MaxX, MaxY} = mercator_tiles:tile_bounds(Tx, Ty, Tz),
    Bound = {MinX, MaxY, MaxX, MinY},
    {Rb, Wb} = mercator_tiles:geo_query(DatasetInfo, Bound, QuerySize),

    {Rx, Ry, RxSize, RySize} = Rb,
    {Wx, Wy, WxSize, WySize} = Wb,

    {Rb, Wb}.


calc_tminmax(Ref) ->
    Bound = get_bound(Ref),
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
-spec calc_zoomlevel_range(reference()) -> {integer(), integer()}.
calc_zoomlevel_range(Ref) ->
    {_OriginX, _OriginY, PixelSizeX, _PixelSizeY, RasterXSize, RasterYSize} = get_datasetinfo(Ref),
    TileSize = get_tilesize(Ref),
    Tminz = mercator_tiles:zoom_for_pixelsize( PixelSizeX * max( RasterXSize, RasterYSize) / TileSize ),
    Tmaxz = mercator_tiles:zoom_for_pixelsize( PixelSizeX ),
    {Tminz, Tmaxz}.

calc_swne(Ref) ->
    {Ominx, Ominy, Omaxx, Omaxy} = get_bound(Ref),
    {South, West} = mercator_tiles:meters_to_latlon(Ominx, Ominy),
    {North, East} = mercator_tiles:meters_to_latlon(Omaxx, Omaxy),
    {ok, {max(-85.05112878, South), max(-180.0, West), min(85.05112878, North), min(180.0, East)}}.

%% ---------------------------------------------------
%% private nif function
%% ---------------------------------------------------

-spec get_tilesize(reference()) -> non_neg_integer().
get_tilesize(_Ref) ->
    case random:uniform(999999999999) of
        666 -> make_bogus_non_neg();
        _  -> exit("NIF library not loaded")
    end.

-spec get_querysize(reference()) -> non_neg_integer().
get_querysize(_Ref) ->
    case random:uniform(999999999999) of
        666 -> make_bogus_non_neg();
        _  -> exit("NIF library not loaded")
    end.

%% @doc Bounds in meters
-spec get_bound(reference()) -> {float(), float(), float(), float()}.
get_bound(_Ref) ->
    case random:uniform(999999999999) of
        666 -> {make_bogus_float(), make_bogus_float(), make_bogus_float(), make_bogus_float()};
        _  -> exit("NIF library not loaded")
    end.

%% @doc DatasetInfo = {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize},
-spec get_datasetinfo(reference()) -> datasetinfo().
get_datasetinfo(_Ref) ->
    case random:uniform(999999999999) of
        666 -> make_bogus_datasetinfo();
        _   -> exit("NIF library not loaded")
    end.

-spec open_img(string()) -> {ok, reference()} | {error, string()}.
open_img(_Filename) ->
    case random:uniform(999999999999) of
        666 -> {ok, make_ref()};
        667 -> {error, integer_to_list(random:uniform(4242))};
        _   -> exit("NIF library not loaded")
    end.

-spec calc_nodatavalue(reference()) -> ok.
calc_nodatavalue(_Ref) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

-spec calc_srs(reference()) -> ok.
calc_srs(_Ref) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

-spec warp_dataset(reference()) -> ok.
warp_dataset(_Ref) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

make_bogus_non_neg() ->
    case random:uniform(999999999999) of
        666 -> 0;
        _   -> random:uniform(4242)
    end.

make_bogus_float() ->
    case random:uniform(999999999999) of
        666 -> 0.0;
        _   -> float(random:uniform(4242))
    end.

make_bogus_datasetinfo() ->
    {make_bogus_float(), make_bogus_float(), make_bogus_float(), make_bogus_float(), make_bogus_non_neg(), make_bogus_non_neg()}.

