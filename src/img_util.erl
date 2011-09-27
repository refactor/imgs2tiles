-module(img_util).

-export([
        calc_zoomlevel_range/1,
        calc_tminmax/1
        ,calc_swne/1
    ]).

-include("gdal2tiles.hrl").

%% @doc Get the minimal and maximal zoom level
%% minimal zoom level: map covers area equivalent to one tile
%% maximal zoom level: closest possible zoom level up on the resolution of raster
-spec calc_zoomlevel_range(imghandler()) -> {byte(), byte()}.
calc_zoomlevel_range({_Ref, RasterInfo, _SizeInfo}) ->
    {_OriginX, _OriginY, PixelSizeX, _PixelSizeY, RasterXSize, RasterYSize} = RasterInfo,
    Tminz = mercator_tiles:zoom_for_pixelsize( PixelSizeX * max( RasterXSize, RasterYSize) / ?TILE_SIZE ),
    Tmaxz = mercator_tiles:zoom_for_pixelsize( PixelSizeX ),
    {Tminz, Tmaxz}.


-spec calc_tminmax(rasterinfo()) -> list().
calc_tminmax(RasterInfo) ->
    Enclosure = get_enclosure(RasterInfo),
    calc_tminmax(Enclosure, [], 0).


-spec calc_swne(RasterInfo::rasterinfo()) -> {ok, {float(), float(), float(), float()}}.
calc_swne(RasterInfo) ->
    {Ominx, Ominy, Omaxx, Omaxy} = get_enclosure(RasterInfo),
    {South, West} = mercator_tiles:meters_to_latlon(Ominx, Ominy),
    {North, East} = mercator_tiles:meters_to_latlon(Omaxx, Omaxy),
    {ok, {max(-85.05112878, South), max(-180.0, West), min(85.05112878, North), min(180.0, East)}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

calc_tminmax(_Enclosure, Tminmax, 32) ->
    lists:reverse(Tminmax);
calc_tminmax({Ominx, Ominy, Omaxx, Omaxy} = Enclosure, Tminmax, Zoom) ->
    {Tminx, Tminy} = mercator_tiles:meters_to_tile( Ominx, Ominy, Zoom ),
    {Tmaxx, Tmaxy} = mercator_tiles:meters_to_tile( Omaxx, Omaxy, Zoom ),
    Z = trunc(math:pow(2, Zoom)) - 1,
    EnclosureOfZoom = {
        max(0, Tminx), max(0, Tminy), 
        min(Z, Tmaxx), min(Z, Tmaxy)
    },
    calc_tminmax(Enclosure, [EnclosureOfZoom|Tminmax], Zoom + 1).


-spec get_enclosure(rasterinfo()) -> enclosure().
get_enclosure(RasterInfo) ->
    {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize} = RasterInfo,
    ExtX = OriginX + PixelSizeX * RasterXSize,
    ExtY = OriginY + PixelSizeY * RasterYSize,
    {min(OriginX, ExtX), min(OriginY, ExtY), max(OriginX, ExtX), max(OriginY, ExtY)}.

    
