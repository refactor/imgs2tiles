%% -------------------------------------------------------------------
%% tile_saver: used to save(or send) finished tile to disk or other system
%%
%% -------------------------------------------------------------------
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
-module(tile_saver).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([save/1]).

%% for debug
-export([do_gc/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { tiles_dir,
                 tile_file_ext}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    {ok, DefaultTilesDir} = application:get_env(imgs2tiles, default_tiles_dir),
    {ok, DefaultTileFileExt} = application:get_env(imgs2tiles, default_tilefile_ext),
    SaveState = #state{tiles_dir=DefaultTilesDir, tile_file_ext=DefaultTileFileExt},
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SaveState], []).

save({_Tile, _Tx, _Ty, _Tz} = TileInfo) ->
    gen_server:cast(?SERVER, {save, TileInfo}).

do_gc() ->
    gen_server:call(?SERVER, do_gc).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([SaveState]) ->
    io:format("~p init args: ~p~n", [?MODULE, SaveState]),
    {ok, SaveState}.

handle_call(do_gc, _From, State) ->
    io:format("Forces an immediate garbage collection of the currently process(~p) of ~p~n", [self(), ?MODULE]),
    erlang:garbage_collect(),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({save, {Tile, Tx, Ty, Tz} = _TileInfo}, State) ->
    SaveTilesToDir = State#state.tiles_dir,
    TilesFileExt = State#state.tile_file_ext,
    TileFilename = filename:join([SaveTilesToDir, 
            integer_to_list(Tz), integer_to_list(Tx), 
            integer_to_list(Ty) ++ "." ++ TilesFileExt]),
    %% Create directories for the tile
    ok = filelib:ensure_dir(TileFilename),
    io:format("saved tile(~p) by process: ~p~n", [TileFilename, self()]),
    gdal_nifs:save_tile(Tile, TileFilename),
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

