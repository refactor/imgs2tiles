-module(gdal_nifs).

-export([init/0]).
-export([open/1,
        close/1]).
-export([get_meta/1]).
-export([get_bound/1, get_pixelsize/1, get_rastersize/1, get_origin/1]).
-export([calc_zoomlevel_range/1, calc_swne/1, calc_tminmax/1]).
-export([geo_query/2]).

-on_load(init/0).

init() ->
    erlang:load_nif("./gdal_nifs", 0).

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

%% @doc For given dataset and query in cartographic coordinates returns parameters for ReadRaster() in raster coordinates and
%% x/y shifts (for border tiles). If the querysize is not given, the extent is returned in the native resolution of dataset ds.
%%
%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = Bound
-spec geo_query(reference(), {float(), float(), float(), float()}) -> 
    {{integer(), integer(), integer(), integer()}, {integer(), integer(), integer(), integer()}}.
geo_query(Ref, Bound) ->
    {OriginX, OriginY} = get_origin(Ref),
    {PixelSizeX, PixelSizeY} = get_pixelsize(Ref),
    {RasterXSize, RasterYSize} = get_rastersize(Ref),
    mercator_tiles:geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY}, {RasterXSize, RasterYSize}, Bound).



%% ---------------------------------------------------
%% private function
%% ---------------------------------------------------

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
calc_zoomlevel_range(Ref) ->
    {RasterXSize, RasterYSize} = get_rastersize(Ref),
    {PixelXSize, _PixelYSize} = get_pixelsize(Ref),
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
-spec get_bound(reference()) -> {float(), float(), float(), float()}.
get_bound(_Ref) ->
    case random:uniform(999999999999) of
        666 -> {make_bogus_float(), make_bogus_float(), make_bogus_float(), make_bogus_float()};
        _  -> exit("NIF library not loaded")
    end.

-spec get_origin(reference()) -> {float(), float()}.
get_origin(_Ref) ->
    case random:uniform(999999999999) of
        666 -> {make_bogus_float(), make_bogus_float()};
        _   -> exit("NIF library not loaded")
    end.

-spec get_pixelsize(reference()) -> {integer(), integer()}.
get_pixelsize(_Ref) ->
    case random:uniform(999999999999) of
        666 -> {make_bogus_non_neg(), make_bogus_non_neg()};
        _   -> exit("NIF library not loaded")
    end.

get_rastersize(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).

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
