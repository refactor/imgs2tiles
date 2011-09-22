%%% -------------------------------------------------------------------
%%% @author wulei <mjollnir.ray@gmail.com>
%%% @doc Purpose:  Convert a raster into TMS (Tile Map Service) tiles in a directory or 
%%%              something else as fast as possible.
%%%           - support of global tiles (Spherical Mercator) for compatibility
%%%               with interactive web maps such as Google Maps
%%% 
%%% this is a clone implementent of gdal2tiles.py, but use elang do some 
%%% parallel work for fast speed 
%%% 
%%% gdal2tiles.py is the work of Klokan Petr Pridal, klokan at klokan dot cz
%%%      Web:      http://www.klokan.cz/projects/gdal2tiles/
%%% @end
%%% -------------------------------------------------------------------
%%  Copyright (c) 2011
%%
%%   Permission is hereby granted, free of charge, to any person obtaining a
%%   copy of this software and associated documentation files (the "Software"),
%%   to deal in the Software without restriction, including without limitation
%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%   and/or sell copies of the Software, and to permit persons to whom the
%%   Software is furnished to do so, subject to the following conditions:
%%   
%%   The above copyright notice and this permission notice shall be included
%%   in all copies or substantial portions of the Software.
%%   
%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%%   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%   DEALINGS IN THE SOFTWARE.
%% -------------------------------------------------------------------
-module(gdal_nifs).

-export([init/0]).
-export([open/1,
        generate_base_tiles/1,
        close/1]).

-export([get_meta/1]).

-export([calc_zoomlevel_range/1, 
         copyout_tile/3,
         build_tile/1,
         save_tile/2,
         calc_swne/1, 
         calc_tminmax/1]).

-include("gdal2tiles.hrl").

%% QuerySize: How big should be query window be for scaling down
%% Later on reset according the chosen resampling algorightm
-type sizeinfo() :: {QuerySize::non_neg_integer(), TileSize::non_neg_integer()}.

-type img()  :: reference().
-type tile() :: reference().

-type imghandler() :: {img(), rasterinfo(), sizeinfo()}.

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


-spec(open(Filename::string()) -> {ok, imghandler()} | {error, string()}).
open(Filename) ->
    case open_img(Filename) of
        {ok, Hdataset} = _Res->
            calc_nodatavalue(Hdataset),
            calc_srs(Hdataset),
            {ok, RasterInfo} = warp_dataset(Hdataset),
            _DataBandsCount = calc_data_bandscount(Hdataset),
            {ok, {Hdataset, RasterInfo, {4 * ?TILE_SIZE, ?TILE_SIZE}}};
        {error, _} = Err ->
            Err
    end.


-spec close(imghandler()) -> ok.
close({Ref, _DI, _SI} = _ImgHandler) ->
    close_img(Ref).


-spec close_img(reference()) -> ok.
close_img(_Ref) ->
    case random:uniform(999999999999) of
        666 -> ok;
        667 -> exit("NIF library not loaded")
    end.


get_meta(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).


%% @doc Generation of the base tiles (the lowest in the pyramid) directly from the input raster
-spec generate_base_tiles(imghandler()) -> ok.
generate_base_tiles({_Ref, RasterInfo, _SizeInfo} = ImgHandler) ->
    %%  LOG("Generating Base Tiles:");
    {_Tminz, Tmaxz} = calc_zoomlevel_range(ImgHandler),
    Tminmax = calc_tminmax(RasterInfo),
    %% Set the bounds
    {Tminx, Tminy, Tmaxx, Tmaxy} = lists:nth(Tmaxz + 1, Tminmax),
    _TCount = (1 + abs(Tmaxx - Tminx)) * (1 + abs(Tmaxy - Tminy)),

    io:format("Tmaxy: ~p, Tminy: ~p, Tminx: ~p, Tmaxx: ~p, Tmaxz: ~p~n", [Tmaxy, Tminy, Tminx, Tmaxx, Tmaxz]),
    generate_tiles_alone_y(Tmaxy, Tminy - 1, Tminx, Tmaxx + 1, Tmaxz, ImgHandler).


%% ---------------------------------------------------
%% private functions
%% ---------------------------------------------------
-spec generate_tiles_alone_y(integer(), integer(), integer(), integer(), byte(), imghandler()) -> ok.
generate_tiles_alone_y(Tminy, Tminy, _Tminx, _Tmaxx, _Tmaxz, _ImgHandler) ->
    ok;
generate_tiles_alone_y(Ty, Tminy, Tminx, Tmaxx, Tmaxz, ImgHandler) ->
    generate_tiles_alone_x(Ty, Tminx, Tmaxx, Tmaxz, ImgHandler),
    generate_tiles_alone_y(Ty - 1, Tminy, Tminx, Tmaxx, Tmaxz, ImgHandler).


-spec generate_tiles_alone_x(integer(), integer(), integer(), byte(), imghandler()) -> ok.
generate_tiles_alone_x(_Ty, Tmaxx, Tmaxx, _Tmaxz, _ImgHandler) ->
    ok;
generate_tiles_alone_x(Ty, Tx, Tmaxx, Tmaxz, ImgHandler) ->
    {ok, Tile} = copyout_tile_for(Ty, Tx, Tmaxz, ImgHandler),
    tile_builder:build(Tile, {Tx, Ty, Tmaxz}),
    generate_tiles_alone_x(Ty, Tx + 1, Tmaxx, Tmaxz, ImgHandler).


-spec copyout_tile_for(integer(), integer(), byte(), imghandler()) -> {ok, tile()} | {error, string()}.
copyout_tile_for(Ty, Tx, Tz, {Img, RasterInfo, {QuerySize, _TileSize}} = _ImgHandler) ->
    %% Tile bounds in EPSG:900913
    {MinX, MinY, MaxX, MaxY} = mercator_tiles:tile_enclosure(Tx, Ty, Tz),
    Bound = {MinX, MaxY, MaxX, MinY},

    {Rb, Wb} = mercator_tiles:geo_query(RasterInfo, Bound, QuerySize),

    copyout_tile(Img, Rb, Wb).


calc_tminmax(RasterInfo) ->
    Enclosure = get_enclosure(RasterInfo),
    calc_tminmax(Enclosure, [], 0).


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

    
%% @doc Get the minimal and maximal zoom level
%% minimal zoom level: map covers area equivalent to one tile
%% maximal zoom level: closest possible zoom level up on the resolution of raster
-spec calc_zoomlevel_range(imghandler()) -> {byte(), byte()}.
calc_zoomlevel_range({_Ref, RasterInfo, _SizeInfo}) ->
    {_OriginX, _OriginY, PixelSizeX, _PixelSizeY, RasterXSize, RasterYSize} = RasterInfo,
    Tminz = mercator_tiles:zoom_for_pixelsize( PixelSizeX * max( RasterXSize, RasterYSize) / ?TILE_SIZE ),
    Tmaxz = mercator_tiles:zoom_for_pixelsize( PixelSizeX ),
    {Tminz, Tmaxz}.


-spec calc_swne(RasterInfo::rasterinfo()) -> {ok, {float(), float(), float(), float()}}.
calc_swne(RasterInfo) ->
    {Ominx, Ominy, Omaxx, Omaxy} = get_enclosure(RasterInfo),
    {South, West} = mercator_tiles:meters_to_latlon(Ominx, Ominy),
    {North, East} = mercator_tiles:meters_to_latlon(Omaxx, Omaxy),
    {ok, {max(-85.05112878, South), max(-180.0, West), min(85.05112878, North), min(180.0, East)}}.


%% ---------------------------------------------------
%% private stub nif functions
%% ---------------------------------------------------

-spec open_img(string()) -> {ok, img()} | {error, string()}.
open_img(_Filename) ->
    erlang:nif_error(nif_not_loaded).

-spec calc_nodatavalue(reference()) -> ok.
calc_nodatavalue(_Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec calc_srs(reference()) -> ok.
calc_srs(_Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec calc_data_bandscount(reference()) -> non_neg_integer().
calc_data_bandscount(_Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec copyout_tile(img(), bandregion(), bandregion()) -> {ok, tile()} | {error, string()}.
copyout_tile(_Img, _R, _W) ->
    erlang:nif_error(nif_not_loaded).

-spec build_tile(Tile::tile()) -> ok.
build_tile(_Tile) ->
    erlang:nif_error(nif_not_loaded).

-spec save_tile(Tile::tile(), TileFileName::string()) -> ok.
save_tile(_Tile, _TileFileName) ->
    erlang:nif_error(nif_not_loaded).

-spec warp_dataset(reference()) -> {ok, rasterinfo()}.
warp_dataset(_Ref) ->
    erlang:nif_error(nif_not_loaded).

