%%% -------------------------------------------------------------------
%%% @author wulei <mjollnir.ray@gmail.com>
%%% @copyright 2011
%%% @doc tile_builder: used to build finished base-tile from primal tile the primal 
%%%             tile, which has orignal copyout tile-data and alpha band, 
%%%             is just copyouted from original raster-image
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
-module(tile_builder).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([
        build/2
        ]).

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

build(Tile, {Tx, Ty, Tz}) ->
    gen_server:cast(?SERVER, {build, Tile, {Tx, Ty, Tz}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({build, Tile, {Tx, Ty, Tz}}, State) ->
    gdal_nifs:build_tile(Tile),
    io:format("built tile(Tx: ~p, Ty: ~p, Tz: ~p) in process: ~p~n", [Tx, Ty, Tz, self()]),
    TileInfo = {Tile, Tx, Ty, Tz},
    tile_saver:save(TileInfo),
    tile_collector:reduce_tile(TileInfo),
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

