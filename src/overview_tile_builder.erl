-module(overview_tile_builder).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([generate/2]).

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

generate(ParentQuadtree, TileList) ->
    gen_server:cast(?SERVER, {generate, ParentQuadtree, TileList}).

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

handle_cast({generate, ParentQuadtree, TileList}, State) ->
    io:format("generate overview parent quadtree: ~p, ~p~n", [ParentQuadtree, TileList]),
    {"0", {Tile0, Tx,Ty,Z}} = lists:keyfind("0", 1, TileList),
    {"1", {Tile1, _,_,Z}} = lists:keyfind("1", 1, TileList),
    {"2", {Tile2, _,_,Z}} = lists:keyfind("2", 1, TileList),
    {"3", {Tile3, _,_,Z}} = lists:keyfind("3", 1, TileList),
    {ok, Tile} = gdal_nifs:generate_overview_tile(Tile0, Tile1, Tile2, Tile3),
    TileInfo = {Tile, Tx div 2, Ty div 2, Z - 1},
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

