-module(gdal_nifs).

-export([init/0]).
-export([open/1,
        close/1]).
-export([get_meta/1]).
-export([calc_zoomlevel_range/1, 
         cut_tile/4,
         calc_swne/1, 
         calc_tminmax/1]).
-export([generate_base_tiles/1]).

-include("gdal2tiles.hrl").

-define(OUTPUT, "/tmp").
-define(TILE_EXT, "png").

% QuerySize: How big should be query window be for scaling down
% Later on reset according the chosen resampling algorightm
-type sizeinfo() :: {QuerySize::non_neg_integer(), TileSize::non_neg_integer()}.
-type imghandler() :: {reference(), datasetinfo(), sizeinfo()}.

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
            {ok, DatasetInfo} = warp_dataset(Hdataset),
            _DataBandsCount = calc_data_bandscount(Hdataset),
            {ok, {Hdataset, DatasetInfo, {4 * ?TILE_SIZE, ?TILE_SIZE}}};
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
-spec generate_base_tiles(imghandler()) -> ok.
generate_base_tiles({_Ref, DatasetInfo, _SizeInfo} = ImgHandler) ->
    %    LOG("Generating Base Tiles:");
    {_Tminz, Tmaxz} = calc_zoomlevel_range(ImgHandler),
    Tminmax = calc_tminmax(DatasetInfo),
    % Set the bounds
    {Tminx, Tminy, Tmaxx, Tmaxy} = lists:nth(Tmaxz + 1, Tminmax),
    _TCount = (1 + abs(Tmaxx - Tminx)) * (1 + abs(Tmaxy - Tminy)),

    generate_tiles_alone_y(Tmaxy, Tminy - 1, Tminx, Tmaxx + 1, Tmaxz, ImgHandler).


%% ---------------------------------------------------
%% private function
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
    cut_tile_for(Ty, Tx, Tmaxz, ImgHandler),
    generate_tiles_alone_x(Ty, Tx + 1, Tmaxx, Tmaxz, ImgHandler).


-spec cut_tile_for(integer(), integer(), byte(), imghandler()) -> ok.
cut_tile_for(Ty, Tx, Tz, {Ref, DatasetInfo, {QuerySize, _TileSize}} = _ImgHandler) ->
    % Create directories for the tile
    file:make_dir(filename:join([?OUTPUT, integer_to_list(Tz)])),
    file:make_dir(filename:join([?OUTPUT, integer_to_list(Tz), integer_to_list(Tx)])),
    
    TileFilename = filename:join([?OUTPUT, integer_to_list(Tz), integer_to_list(Tx), integer_to_list(Ty) ++ "." ++ ?TILE_EXT]),
    io:format("tilefilename: ~p~n", [TileFilename]),

    % Tile bounds in EPSG:900913
    {MinX, MinY, MaxX, MaxY} = mercator_tiles:tile_enclosure(Tx, Ty, Tz),
    Bound = {MinX, MaxY, MaxX, MinY},

    {Rb, Wb} = mercator_tiles:geo_query(DatasetInfo, Bound, QuerySize),

    io:format("ReadRaster Extend: ~p ~p~n", [Rb, Wb]),
    
    cut_tile(Ref, Rb, Wb, TileFilename).


calc_tminmax(DatasetInfo) ->
    Enclosure = get_enclosure(DatasetInfo),
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

-spec get_enclosure(datasetinfo()) -> enclosure().
get_enclosure(DatasetInfo) ->
    {OriginX, OriginY, PixelSizeX, PixelSizeY, RasterXSize, RasterYSize} = DatasetInfo,
    ExtX = OriginX + PixelSizeX * RasterXSize,
    ExtY = OriginY + PixelSizeY * RasterYSize,
    {min(OriginX, ExtX), min(OriginY, ExtY), max(OriginX, ExtX), max(OriginY, ExtY)}.

    
%% @doc Get the minimal and maximal zoom level
%% minimal zoom level: map covers area equivalent to one tile
%% maximal zoom level: closest possible zoom level up on the resolution of raster
-spec calc_zoomlevel_range(imghandler()) -> {byte(), byte()}.
calc_zoomlevel_range({_Ref, DatasetInfo, _SizeInfo}) ->
    {_OriginX, _OriginY, PixelSizeX, _PixelSizeY, RasterXSize, RasterYSize} = DatasetInfo,
    Tminz = mercator_tiles:zoom_for_pixelsize( PixelSizeX * max( RasterXSize, RasterYSize) / ?TILE_SIZE ),
    Tmaxz = mercator_tiles:zoom_for_pixelsize( PixelSizeX ),
    {Tminz, Tmaxz}.

calc_swne(DatasetInfo) ->
    {Ominx, Ominy, Omaxx, Omaxy} = get_enclosure(DatasetInfo),
    {South, West} = mercator_tiles:meters_to_latlon(Ominx, Ominy),
    {North, East} = mercator_tiles:meters_to_latlon(Omaxx, Omaxy),
    {ok, {max(-85.05112878, South), max(-180.0, West), min(85.05112878, North), min(180.0, East)}}.

%% ---------------------------------------------------
%% private nif function
%% ---------------------------------------------------

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

calc_data_bandscount(_Ref) ->
    case random:uniform(999999999999) of
        666 -> make_bogus_non_neg();
        _   -> exit("NIF library not loaded")
    end.

-spec cut_tile(reference(), bandregion(), bandregion(), string()) -> ok.
cut_tile(_Ref, _R, _W, _FileName) ->
    ok.

-spec warp_dataset(reference()) -> {ok, datasetinfo()}.
warp_dataset(_Ref) ->
    case random:uniform(999999999999) of
        666 -> {ok, make_bogus_datasetinfo()};
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

