-module(mercator_tiles).

-export([latlon_to_meters/2, meters_to_latlon/2, meters_to_tile/3, tile_bounds/3, tile_latlon_bounds/3, zoom_for_pixelsize/1]).
-export([resolution/1]).
-export([quadtree/3]).
-export([geo_query/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MAXZOOMLEVEL, 32).
-define(TILE_SIZE, 256).
-define(EARTH_RADIUS, 6378137).
-define(PI, math:pi()).
-define(ORIGIN_SHIFT, (2 * ?PI * ?EARTH_RADIUS / 2.0)).
-define(INITIAL_RESOLUTION, (2 * ?PI * ?EARTH_RADIUS / ?TILE_SIZE)).

%% @doc Converts given Lat/Lon in WGS84 Datum to XY in Spherical/Web Mercator EPSG:900913
-spec(latlon_to_meters(Lat::float(), Lon::float()) -> {float(), float()}).
latlon_to_meters(Lat, Lon) ->
    MX = Lon * ?ORIGIN_SHIFT / 180.0,
    Y = math:log( math:tan((90 + Lat) * ?PI / 360.0)) / (?PI / 180.0),
    MY = Y * ?ORIGIN_SHIFT / 180.0,
    {MX, MY}.

%% @doc Converts XY point from Spherical Mercator EPSG:900913 to lat/lon in WGS84 Datum
-spec(meters_to_latlon(MX::float(), MY::float()) -> {float(), float()}).
meters_to_latlon(MX, MY) ->
    Lon = (MX / ?ORIGIN_SHIFT ) * 180.0,
    Lt =  (MY / ?ORIGIN_SHIFT) * 180.0,
    Lat = 180 / ?PI * (2 * math:atan( math:exp( Lt * ?PI / 180.0) ) - ?PI / 2.0),
    {Lat, Lon}.

%% @doc Converts pixel coordinates in given zoom level of pyramid to EPSG:900913
-spec(pixels_to_meters(PX::non_neg_integer(), PY::non_neg_integer(), Zoom::byte()) -> {float(), float()}).
pixels_to_meters(PX, PY, Zoom) ->
    Resolution = resolution(Zoom),
    MX = PX * Resolution - ?ORIGIN_SHIFT,
    MY = PY * Resolution - ?ORIGIN_SHIFT,
    {MX, MY}.

%% @doc Returns tile for given mercator coordinates
-spec(meters_to_tile(MX::float(), MY::float(), Zoom::byte()) -> {integer(), integer()}).
meters_to_tile(MX, MY, Zoom) ->
    {PX, PY} = meters_to_pixels(MX, MY, Zoom),
    pixels_to_tile(PX, PY).

%% @doc Returns bounds of the given tile in EPSG:900913 coordinates
-spec(tile_bounds(TX::non_neg_integer(), TY::non_neg_integer(), Zoom::byte()) -> {float(),float(),float(),float()}).
tile_bounds(TX, TY, Zoom) ->
    {MinX, MinY} = pixels_to_meters(TX * ?TILE_SIZE, TY * ?TILE_SIZE, Zoom),
    {MaxX, MaxY} = pixels_to_meters((TX + 1) * ?TILE_SIZE, (TY + 1) * ?TILE_SIZE, Zoom),
    {MinX, MinY, MaxX, MaxY}.

%% @doc Returns bounds of the given tile in latutude/longitude using WGS84 datum
-spec(tile_latlon_bounds(TX::non_neg_integer(), TY::non_neg_integer(), Zoom::byte()) -> {float(),float(),float(),float()}).
tile_latlon_bounds(TX, TY, Zoom) ->
    {MinX, MinY, MaxX, MaxY} = tile_bounds(TX, TY, Zoom),
    {MinLat, MinLon} = meters_to_latlon(MinX, MinY),
    {MaxLat, MaxLon} = meters_to_latlon(MaxX, MaxY),
    {MinLat, MinLon, MaxLat, MaxLon}.

%% @doc Maximal scaledown zoom of the pyramid closest to the pixelSize.
-spec(zoom_for_pixelsize(PixelSize::float()) -> byte()).
zoom_for_pixelsize(PixelSize) ->
    zoom_for_pixelsize(PixelSize, 0).

%% @doc Resolution (meters/pixel) for given zoom level (measured at Equator)
-spec(resolution(Zoom::byte()) -> float()).
resolution(Zoom) ->
    ?INITIAL_RESOLUTION / math:pow(2, Zoom).

%% @doc Converts TMS tile coordinates to Microsoft QuadTree
-spec(quadtree(TX::non_neg_integer(), TY::non_neg_integer(), Zoom::byte()) -> string()).
quadtree(TX, TY, Zoom) ->
    Ty = trunc(math:pow(2, Zoom) - 1 - TY),
    quadtree(TX, Ty, Zoom, "").

%% {LeftTopX, LeftTopY, RightBottomX, RightBottomY} = _Bound
-spec geo_query({float(), float(), float(), float()}, {non_neg_integer(), non_neg_integer()}, {float(), float(), float(), float()}, non_neg_integer()) ->
    {{integer(), integer(), integer(), integer()}, {integer(), integer(), integer(), integer()}}.
geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY} =_OP, {RasterXSize, RasterYSize} =_RS, {Ulx, Uly, Lrx, Lry} = _Bound, QuerySize) ->
    Rx = trunc( (Ulx - OriginX) / PixelSizeX + 0.001),
    Ry = trunc( (Uly - OriginY) / PixelSizeY + 0.001),
    Rxsize = trunc( (Lrx - Ulx) / PixelSizeX + 0.5),
    Rysize = trunc( (Lry - Uly) / PixelSizeY + 0.5),

    {NewRx, NewWx, ResWxsize, ResRxsize} = adjust_byedge(Rx, Rxsize, RasterXSize, QuerySize),
    {NewRy, NewWy, ResWysize, ResRysize} = adjust_byedge(Ry, Rysize, RasterYSize, QuerySize),

    {{NewRx, NewRy, ResRxsize, ResRysize}, {NewWx, NewWy, ResWxsize, ResWysize}}.

%% ===================================================================
%% private funcs
%% ===================================================================

%% @doc Coordinates should not go out of the bounds of the raster
-spec adjust_byedge(integer(), integer(), integer(), non_neg_integer()) -> {integer(), integer(), integer(), integer()}.
adjust_byedge(R, Rsize, RasterSize, QuerySize) ->
    if
        QuerySize == 0 ->
            Wsize0 = Rsize;
        true ->
            Wsize0 = QuerySize
    end,

    {NewR, NewW, NewWsize, NewRsize} = 
        if R < 0 ->
                Rshift = abs(R),
                W = trunc( Wsize0 * (Rshift / Rsize) ),
                Wsize = Wsize0 - W,
                {0, W, Wsize, Rsize - trunc(Rsize * (Rshift / Rsize)) };
            true ->
                {R, 0, Wsize0, Rsize}
        end,

    {ResWsize, ResRsize} = 
        if
            R + Rsize > RasterSize ->
                {trunc( NewWsize * (RasterSize - NewR) / NewRsize), RasterSize - NewR};
            true ->
                {NewWsize, NewRsize}
        end,
    {NewR, NewW, ResWsize, ResRsize}.

quadtree(_TX, _TY, 0, Quadtree) -> 
    Quadtree;
quadtree(TX, TY, Zoom, Quadtree) -> 
    Mask = 1 bsl (Zoom - 1),
    Digit = bit_op(TX, TY, Mask),
    quadtree(TX, TY, Zoom - 1, Quadtree ++ integer_to_list(Digit)).

bit_op(TX, TY, Mask) ->
    R1 = 
    if
        TX band Mask =/= 0 ->
            1;
        true ->
            0
    end,
    R2 =
    if
        TY band Mask =/= 0 ->
            2;
        true ->
            0
    end,
    R1 + R2.

zoom_for_pixelsize(PixelSize, I) ->
    R = resolution(I),
    if 
        PixelSize > R ->
            cfi(I);
        I < ?MAXZOOMLEVEL ->
            zoom_for_pixelsize(PixelSize, I + 1);
        true ->
            I
    end.


cfi(I) ->
    case I of
        0 -> 0;
        _ -> I - 1
    end.


%% @doc Converts EPSG:900913 to pyramid pixel coordinates in given zoom level
meters_to_pixels(MX, MY, Zoom) ->
    Resolution = resolution(Zoom),
    PX = (MX + ?ORIGIN_SHIFT) / Resolution,
    PY = (MY + ?ORIGIN_SHIFT) / Resolution,
    {PX, PY}.

%% @doc Returns a tile covering region in given pixel coordinates
pixels_to_tile(PX, PY) ->
    TX = math_utils:ceiling( PX / ?TILE_SIZE ) - 1,
    TY = math_utils:ceiling( PY / ?TILE_SIZE ) - 1,
    {TX, TY}.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
latlon_to_meters_test_() ->
    [
        ?_test(math_utils:xy_assert({13247019.4043996,4865942.27950318}, latlon_to_meters(40,119))),
        ?_test(math_utils:xy_assert({14360214.312332291, 2273030.9269876881}, latlon_to_meters(20, 129))),
        ?_test(math_utils:xy_assert({13469658.385986103, -4721671.5725801103}, latlon_to_meters(-39, 121)))
    ].

meters_to_latlon_test_() ->
    [
        ?_test(math_utils:xy_assert({-39, 121}, meters_to_latlon(13469658.385986103, -4721671.5725801103))),
        ?_test(math_utils:xy_assert({40, 119}, meters_to_latlon(13247019.4043996,4865942.27950318))),
        ?_test(math_utils:xy_assert({20, 129}, meters_to_latlon(14360214.312332291, 2273030.9269876881)))
    ].

pixels_to_meters_test_() ->
    [
        ?_test(math_utils:xy_assert({762677661.29741549, 762677661.29741549}, pixels_to_meters(10000, 10000, 1))),
        ?_test(math_utils:xy_assert({-4383204.9499851465, -4383204.9499851465}, pixels_to_meters(100, 100, 0)))
    ].

meters_to_pixels_test_() ->
    [
        ?_test(math_utils:xy_assert({10000, 10000}, meters_to_pixels(762677661.29741549, 762677661.29741549, 1))),
        ?_test(math_utils:xy_assert({1700.9777777777776, 1272.6698849462664}, meters_to_pixels(13247019.404399557, 4865942.2795031769, 3)))
    ].

pixels_to_tile_test() ->
    math_utils:xy_assert({-1, -1}, pixels_to_tile(0, 0)),
    math_utils:xy_assert({3, 3}, pixels_to_tile(1000, 1000)).

meters_to_tile_test() ->
    math_utils:xy_assert({0, 0}, meters_to_tile(1000, 1000, 0)),
    math_utils:xy_assert({26, 19}, meters_to_tile(13247019.404399557, 4865942.2795031769, 5)).

tile_bounds_test() ->
    math_utils:swne_assert({-20037508.342789244, -20037508.342789244, 20037508.342789244, 20037508.342789244}, tile_bounds(0, 0, 0)).

tile_latlon_bounds_test() ->
    math_utils:swne_assert({-85.051128779806589, -180.0, 85.051128779806604, 180.0}, tile_latlon_bounds(0,0,0)),
    math_utils:swne_assert({-84.959304956238341, -171.5625, -84.943836614828442, -171.38671875}, tile_latlon_bounds(48, 6, 11)).

zoom_for_pixelsize_test() ->
    ?assertEqual(0, zoom_for_pixelsize(1000000)),
    ?assertEqual(0, zoom_for_pixelsize(100000)),
    ?assertEqual(20, zoom_for_pixelsize(0.1)),
    ?assertEqual(30, zoom_for_pixelsize(0.0000728964)),
    ?assertEqual(3, zoom_for_pixelsize(10000)).

quadtree_test() ->
    ?assertEqual("22221", quadtree(1, 1, 5)).

geo_quert_test() ->
    {OriginX, OriginY} = {13024084.000533571, 4184269.256414418},
    {PixelSizeX, PixelSizeY} = {0.24473611762142541, -0.24473611762142541},
    {RasterXSize, RasterYSize} = {60352, 62961},
    MinMaxBound = {865067, 633770, 865453, 633367},
    {R, W} = geo_query({OriginX, OriginY, PixelSizeX, PixelSizeY}, {RasterXSize, RasterYSize}, MinMaxBound, 0),
    ?assertEqual({0, 14507459, -49680575, -14444498}, R),
    ?assertEqual({49682152, 0, -49680575, -14444498}, W).
-endif.
