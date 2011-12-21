-module(tile_collect).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([reduce/2, delete/1]).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


reduce(TileInfo, ImgFileName) ->
    gen_server:cast(?SERVER, {reduce, TileInfo, ImgFileName}).


delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("~p init args: ~p~n", [?MODULE, Args]),
    {ok, Args}.

handle_call({delete, Key}, _From, State) ->
    case tc_store:lookup(Key) of
        {ok, Pid} ->
            tc_collector:delete(Pid);
        {error, _Reason} ->
            ok
    end,
    {reply, ok, State}.

handle_cast({reduce, TileInfo, ImgFileName}, State) ->
    {_, Tx, Ty, Tz} = TileInfo,
    {ParentQuadtree, _ChildPosition} = mercator_tiles:parent_quadtree(Tx, Ty, Tz),
    Key = ParentQuadtree,
    Pid2 = 
    case tc_store:lookup(Key) of
        {ok, Pid} ->
            Pid;
        {error, _} ->
            {ok, Pid} = tc_collector:create(TileInfo, ImgFileName),
            tc_store:insert(Key, Pid),
            Pid
    end,
    tc_collector:reduce_tile(Pid2, TileInfo, ImgFileName),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

