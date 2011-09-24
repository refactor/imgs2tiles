%%% -------------------------------------------------------------------
%%% @author wulei <mjollnir.ray@gmail.com>
%%% @copyright 2011
%%% @doc tile_reducer: used to generate overview-tiles
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
-module(tile_reducer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([
        get_count/0,
        generate_overview_tile/1
        ]).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tile_dict}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_count() ->
    gen_server:call(?MODULE, get_count).

generate_overview_tile(TileInfo) ->
    gen_server:cast(?MODULE, {reduce_tiles, TileInfo}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("~p init args: ~p~n", [?MODULE, Args]),
    {ok, #state{ tile_dict = dict:new() }}.

handle_call(get_count, _From, State) ->
    {reply, dict:size(State#state.tile_dict), State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({reduce_tiles, {_Tile, Tx, Ty, Tz} = TileInfo}, State) ->
    io:format("generate_overview_tile Tx: ~p, Ty: ~p, Tz:~p -> by ~p~n", [Tx, Ty, Tz, self()]),
    {ParentQuadtree, ChildPosition} = mercator_tiles:parent_quadtree(Tx, Ty, Tz),
    NewTileDict = dict:update(ParentQuadtree, 
                              fun(ExistsTileList) -> [{ChildPosition, TileInfo} | ExistsTileList] end, 
                              [{ChildPosition, TileInfo}], 
                              State#state.tile_dict),
    {noreply, #state{ tile_dict = NewTileDict }};
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

