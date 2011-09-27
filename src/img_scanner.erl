%%% -------------------------------------------------------------------
%%% @author wulei <mjollnir.ray@gmail.com>
%%% @copyright 2011
%%% @doc img_scanner: used to scan the original raster imagine file and 
%%%              divide it into primal tiles
%%%              the copyout primal tile will send to tile_builder for 
%%%              generating finished tile
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
-module(img_scanner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("gdal2tiles.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([
        generate_base_tiles/1,
        scan_img/1
        ]).

%% for debug
-export([do_gc/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

scan_img(ImgFileName) ->
    gen_server:cast(?SERVER, {scan_img, ImgFileName}).

do_gc() ->
    gen_server:call(?SERVER, do_gc).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(do_gc, _From, State) ->
    io:format("Forces an immediate garbage collection of the currently process(~p) of ~p~n", [self(), ?MODULE]),
    erlang:garbage_collect(),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({scan_img, ImgFileName}, State) ->
    {ok, HI} = gdal_nifs:open(ImgFileName),
    {ElapseTime, ok} = timer:tc(?MODULE, generate_base_tiles, [HI]),
    io:format("scan img time: ~p~n", [ElapseTime]),
    gdal_nifs:close(HI),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc Generation of the base tiles (the lowest in the pyramid) directly from the input raster
-spec generate_base_tiles(imghandler()) -> ok.
generate_base_tiles({_Ref, RasterInfo, _SizeInfo} = ImgHandler) ->
    %%  LOG("Generating Base Tiles:");
    {_Tminz, Tmaxz} = img_util:calc_zoomlevel_range(ImgHandler),
    Tminmax = img_util:calc_tminmax(RasterInfo),
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

    gdal_nifs:copyout_tile(Img, Rb, Wb).



